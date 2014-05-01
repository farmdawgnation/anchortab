package com.anchortab.snippet

import net.liftweb._
  import common._
  import sitemap._
    import Loc._
  import http._
    import SHtml._
    import S.SFuncHolder
    import provider._
    import js._
      import JsCmds._
    import LiftRules._
  import util._
    import Helpers._
  import json._
  import mongodb.BsonDSL._

import com.stripe

import com.anchortab.model._
import com.anchortab.actor._

object registrationCoupon extends RequestVar[Box[stripe.Coupon]](Empty)

object Register {
  val menu = Menu.i("Register") / "register" >>
  Authentication.ifNotLoggedIn

  val promoMenu = Menu.param[stripe.Coupon](
    "promo",
    "promo",
    (s) => registrationCoupon.is or {
      registrationCoupon(tryo(stripe.Coupon.retrieve(s)))
      registrationCoupon.is
    },
    _.id
  ) / "promo" >>
  TestValueAccess((coupon) => {
    println("test value access")
    for (coupon <- coupon) yield {
      if (coupon.valid) {
        RedirectWithState(menu.loc.calcDefaultHref, RedirectState(() => registrationCoupon(Full(coupon))))
      } else {
        RedirectResponse(menu.loc.calcDefaultHref)
      }
    }
  })
}

class Register extends Loggable {
  private var stripeToken = ""
  private var emailAddress = ""
  private var requestedPassword = ""
  private var selectedPlan: Plan = Plan.DefaultPlan

  private val plans = Plan.findAll("visibleOnRegistration" -> true)

  private lazy val hasFullDiscount_? = registrationCoupon.is.map(coupon => {
    coupon.valid && coupon.percentOff == 100
  }).openOr(false)

  private lazy val planSelections = (Plan.DefaultPlan +: plans).map({ plan =>
    SelectableOption(plan, plan.registrationTitle, "data-has-trial" -> (plan.hasTrial_? || plan.free_? || hasFullDiscount_?).toString)
  }).sortWith(_.value.price > _.value.price)

  private def createStripeCustomer(plan: Plan) = {
    def createFn(customerMap: Map[String, _]) = tryo(stripe.Customer.create(customerMap))

    (selectedPlan, stripeToken.trim) match {
      case (freePlan, _) if freePlan.free_? =>
        createFn(Map("email" -> emailAddress))

      case (paidPlan, _) if hasFullDiscount_? =>
        createFn(Map(
          "email" -> emailAddress,
          "coupon" -> registrationCoupon.is.map(_.id).openOr(""),
          "plan" -> plan.stripeId.getOrElse("")
        ))

      case (_, stripeToken) if stripeToken.isEmpty =>
        Failure("Your Stripe Token was empty and you attempted to subscribe to a paid plan.")

      case (paidPlan, _) if paidPlan.stripeId.isEmpty =>
        Failure("Paid plan didn't have a Stripe identifier.")

      case (paidPlan, stripeToken) if registrationCoupon.is.isDefined =>
        createFn(Map(
          "email" -> emailAddress,
          "plan" -> plan.stripeId.getOrElse(""),
          "card" -> stripeToken,
          "coupon" -> registrationCoupon.is.map(_.id).openOr("")
        ))

      case (paidPlan, stripeToken) =>
        createFn(Map(
          "email" -> emailAddress,
          "plan" -> plan.stripeId.getOrElse(""),
          "card" -> stripeToken
        ))
    }
  }

  private def generateSubscriptionForPlan(plan:Plan) = {
    Full(UserSubscription(plan._id, plan.price, plan.term))
  }

  private def processRegistration = {
    val validators = Map(
      "input.email-address" -> (() =>
        if (".+@.+\\..+".r.findAllIn(emailAddress).nonEmpty)
          if (User.countOfUsersWithEmail(emailAddress) > 0) {
            Notices.error("Your email is already registered. Log in below.")
            S.redirectTo(Authentication.managerMenu.loc.calcDefaultHref)
          } else {
            Empty
          }
        else
          Full("A valid email address is required.")
      ),
      "input.password" -> (() =>
        if (requestedPassword.nonEmpty)
          Empty
        else
          Full("Password is required.")
      )
    )

    val validationErrors: List[FormValidationError] = validators.flatMap { validator =>
      val validatorSelector = validator._1
      val validatorFunc = validator._2

      validatorFunc() match {
        case Full(validationError) => Some(FormValidationError(validatorSelector, validationError))
        case _ => None
      }
    }.toList

    validationErrors match {
      case Nil =>
        val user : Box[User] =
          for {
            subscription <- generateSubscriptionForPlan(selectedPlan)
            customer <- createStripeCustomer(selectedPlan)
          } yield {
            val firstSteps = Map(
              UserFirstStep.Keys.ConnectAnExternalService -> UserFirstStep.Steps.ConnectAnExternalService,
              UserFirstStep.Keys.CreateATab -> UserFirstStep.Steps.CreateATab,
              UserFirstStep.Keys.EmbedYourTab -> UserFirstStep.Steps.EmbedYourTab
            )

            val userActiveCard = for {
              defaultCardId <- customer.defaultCard
              card <- customer.cards.data.find(_.id == defaultCardId)
            } yield {
              UserActiveCard(card.last4, card.`type`, card.expMonth, card.expYear)
            }

            def referringAffiliateId = {
              S.cookieValue(Affiliate.cookieName).flatMap { code =>
                User.find("affiliateCode" -> code).map(_._id)
              }
            }

            User(emailAddress, User.hashPassword(requestedPassword),
                 None,
                 subscriptions = List(subscription), firstSteps = firstSteps,
                 stripeCustomerId = Some(customer.id),
                 activeCard = userActiveCard,
                 referringAffiliateId = referringAffiliateId
            )
          }

        user.foreach(_.save)

        user match {
          case Full(user) =>
            val loginResult = Authentication.processLogin(emailAddress, requestedPassword)

            // Send welcome email
            EmailActor ! SendWelcomeEmail(user.email)

            loginResult

          case Failure(message, _, _) =>
            logger.warn("While registering account got: " + message)
            GeneralError("An error occured while creating your account: " + message + " If you continue receiving this error for no apparant reason, please contact us at hello@anchortab.com.")

          case Empty =>
            logger.warn("Got empty while registering account.")
            GeneralError("An error occured while creating your account. If you continue receiving this error for no apparant reason, please contact us at hello@anchortab.com.")
        }

      case errors =>
        errors.foldLeft(Noop)(_ & _)
    }
  }

  def render = {
    if (registrationCoupon.is.isDefined) {
      val percentOff = registrationCoupon.is.map(_.percentOff).openOr(0)
      val duration = registrationCoupon.is.map(_.duration).openOr("") match {
        case "once" => "one month"
        case "repeating" => "multiple months"
        case _ => "forever"
      }

      Notices.notice(s"Coupon applied: $percentOff% off $duration.")
    }

    SHtml.makeFormsAjax andThen
    ".plan-selection" #> selectObj[Plan](planSelections, Empty, selectedPlan = _) &
    "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
    ".email-address" #> text(emailAddress, emailAddress = _) &
    ".password" #> password(requestedPassword, requestedPassword = _) &
    ".submit" #> ajaxSubmit("Register", () => processRegistration)
  }
}
