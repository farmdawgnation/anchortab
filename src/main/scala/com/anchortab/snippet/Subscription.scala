package com.anchortab.snippet

import scala.xml.NodeSeq

import java.text.SimpleDateFormat

import net.liftweb._
  import sitemap._
  import common._
  import http._
    import SHtml._
    import js._
      import JsCmds._
    import LiftRules._
  import json._
    import JsonDSL._
    import Extraction._
  import util._
    import Helpers._
  import mongodb.BsonDSL._

import org.bson.types.ObjectId

import org.joda.time._

import com.anchortab.model._
import com.anchortab.util._

import com.stripe

case class SubmitBillingInfoToStripe(submitBillingInfoFunc: (String)=>JsCmd) extends
  AnonCallableFunction(submitBillingInfoFunc)

case class UpdateBillingInformation(updateStripeTokenFn: JsCmd) extends
  AnchorTabEvent("update-billing-information", ("updateStripeTokenFn" -> updateStripeTokenFn.toJsCmd))
case object ErrorChargingCard extends SimpleAnchorTabEvent("error-charging-card")
case object NoBillingInformationError extends SimpleAnchorTabEvent("no-billing-information-error")
case class GeneralError(errorText: String) extends SimpleAnchorTabEvent("general-error")

object Subscription extends Loggable {
  val subscriptionMenu = Menu.i("Subscription") / "manager" / "subscription"

  val menus =
    subscriptionMenu ::
    Nil

  implicit val formats = DefaultFormats
  val dateFormatter = new SimpleDateFormat("dd MMM yyyy")

  def snippetHandlers : SnippetPF = {
    case "subscription-summary" :: Nil => subscriptionSummary
    case "plan-selection" :: Nil => planSelection
    case "billing-summary" :: Nil => billingSummary
  }

  def subscriptionSummary = {
    {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
        subscription <- user.subscription
        plan <- Plan.find(subscription.planId)
      } yield {
        val planStatus = {
          if (! plan.isSpecial && ! subscription.cancelled_?)
            ".special-plan-assignment" #> ClearNodes andThen
            ".cancelled-subscription" #> ClearNodes
          else if (subscription.cancelled_?)
            ".subscribed" #> ClearNodes andThen
            ".special-plan-assignment" #> ClearNodes
          else
            ".subscribed" #> ClearNodes andThen
            ".cancelled-subscription" #> ClearNodes
        }

        planStatus andThen
        ".trial-plan" #> (subscription.trial_? ? PassThru | ClearNodes) andThen
        ".plan-name *" #> plan.name &
        ".ending-date *" #> subscription.ends.map { subscriptionEndingDate =>
          dateFormatter format subscriptionEndingDate.toDate
        } &
        ".not-subscribed" #> ClearNodes
      }
    } openOr {
      ".cancelled-subscription" #> ClearNodes &
      ".provisional-plan" #> ClearNodes &
      ".special-plan-assignment" #> ClearNodes &
      ".subscribed" #> ClearNodes
    }
  }

  def planSelection = {
    def changeSubscription(user: User, subscription: Option[UserSubscription], newPlanId: ObjectId)() = {
      val changeResult: Box[JsCmd] =
        if (user.activeCard.isDefined) {
          for {
            plan <- (Plan.find(newPlanId):Box[Plan]) ?~ "Plan not found."
            newPlanStripeId <- (plan.stripeId:Box[String]) ?~ "Plan lacks Stripe ID."
            customerId <- (user.stripeCustomerId:Box[String]) ?~ "User lacks Stripe ID."
            customer <- tryo(stripe.Customer.retrieve(customerId)) ?~ "Stripe doesn't recognize user."
            updatedStripeSubscription <- tryo(customer.updateSubscription(Map(
              "plan" -> newPlanStripeId,
              "trial_end" -> "now"
            )))
            updatedSubscription = subscription.map(_.copy(
              status = "cancelled",
              ends = Some(new DateTime())
            ))
            newSubscription = UserSubscription(newPlanId, plan.price, plan.term, status = "active")
          } yield {
            val allButCurrentSubscription = user.subscriptions.filter { sub =>
              sub._id != (updatedSubscription.map(_._id) getOrElse "")
            }

            val newSubscriptions = (updatedSubscription :: Some(newSubscription) :: Nil).flatten
            val updatedUser = user.copy(
              subscriptions = allButCurrentSubscription ++ newSubscriptions
            )

            updatedUser.save

            Authentication.authenticationStickyNotices(updatedUser)

            Notices.notice("Your subscription has been successfully changed.")
            Reload
          }
        } else {
          Full(NoBillingInformationError)
        }

      changeResult match {
        case Full(jsCmd) =>
          jsCmd

        case Empty =>
          ErrorChargingCard

        case Failure(msg, _, _) =>
          logger.error("Error updating subscription: " + msg)
          GeneralError("Something went wrong while attempting to update your subscription: " + msg)
      }
    }

    def cancelSubscription(user: User, subscription: UserSubscription)() = {
      {
        for {
          customerId <- user.stripeCustomerId
          customer <- tryo(stripe.Customer.retrieve(customerId))
          stripeSubscription <- tryo(customer.cancelSubscription(Map(
            "at_period_end" -> true
          )))
          periodEnd <- stripeSubscription.currentPeriodEnd
          updatedSubscription = subscription.copy(
            status = "cancelled",
            ends = Some(new DateTime(periodEnd * 1000))
          )
        } yield {
          val allButCurrentSubscription = user.subscriptions.filter { sub =>
            sub._id != updatedSubscription._id
          }

          val updatedUser = user.copy(
            subscriptions = allButCurrentSubscription ++ (updatedSubscription :: Nil)
          )

          updatedUser.save

          Notices.notice("Your subscription has been cancelled. You will not be billed again.")
          Reload
        }
      } getOrElse {
        GeneralError("Error while canceling subscription. Please contact us.")
      }
    }

    {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
          if ! user.onSpecialPlan_?
      } yield {
        val subscription = user.subscription
        val currentPlan = user.subscription.flatMap { sub =>
          Plan.find(sub.planId)
        } getOrElse Plan.DefaultPlan

        val cancelClick = {
          for {
            sub <- subscription
          } yield {
            cancelSubscription(user, sub) _
          }
        } getOrElse {
          () => Noop
        }

        val plans = Plan.findAll("$or" -> JArray(List(
          ("visibleOnRegistration" -> true),
          ("_id" -> currentPlan._id)
        )))
        val currentSubscriptionCanceling = subscription.map(_.cancelled_?).getOrElse(false)

        ClearClearable andThen
        ".plan" #> plans.map { plan =>
          ".plan-name *" #> plan.registrationTitle &
          ".plan-details *" #> plan.description &
          ".select-plan" #> ((plan._id == currentPlan._id || currentSubscriptionCanceling) ? ClearNodes | PassThru) &
          ".select-plan [onclick]" #> ajaxInvoke(changeSubscription(user, subscription, plan._id) _) &
          ".cancel-plan" #> ((plan._id == currentPlan._id && ! currentSubscriptionCanceling) ? PassThru | ClearNodes) &
          ".cancel-plan [onclick]" #> ajaxInvoke(cancelClick)
        }
      }
    } openOr {
      ClearNodes
    }
  }

  def billingSummary = {
    def submitBillingUpdateToStripe(token: String) = {
      val billingUpdateResult = {
        for {
          session <- userSession.is ?~! "Could not find session."
          user <- (User.find(session.userId): Box[User]) ?~! "Could not find user."
          customerId <- (user.stripeCustomerId: Box[String]) ?~! "We couldn't find your Stripe ID."
          customer <- tryo(stripe.Customer.retrieve(customerId)) ?~! "We couldn't retrieve your customer data."
          updatedCustomer <- tryo(customer.update(Map("card" -> token)))
          card <- (updatedCustomer.activeCard: Box[stripe.Card]) ?~! "Could not find active card."
        } yield {
          User.update("_id" -> user._id, "$set" -> (
            "activeCard" -> decompose(
              UserActiveCard(card.last4, card.`type`, card.expMonth, card.expYear)
            )
          ))
        }
      }

      billingUpdateResult match {
        case Empty =>
          Notices.error("An internal error occured. Please contact us at hello@anchortab.com and let us know.")
          Reload

        case fail @ Failure(msg, _, _) =>
          logger.warn(fail)
          Notices.error("An error occured while updating billing information: " + msg)
          Reload

        case _ =>
          Notices.notice("Your billing information has been successfully updated.")
          Reload
      }
    }

    {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
      } yield {
        ".no-billing-information" #> (user.activeCard.isDefined ? ClearNodes | PassThru) &
        ".billing-information-on-file" #> user.activeCard.map { card =>
          ".card-type *" #> card.cardType &
          ".last4 *" #> card.last4 &
          ".expMonth *" #> card.expMonth &
          ".expYear *" #> card.expYear
        } &
        ".update-billing-information [onclick]" #> ajaxInvoke(() =>
          UpdateBillingInformation(SubmitBillingInfoToStripe(submitBillingUpdateToStripe)))
      }
    } openOr {
      ".billing-information" #> ClearNodes
    }
  }
}
