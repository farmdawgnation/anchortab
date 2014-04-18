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
    import Extraction._
  import util._
    import Helpers._
  import mongodb.BsonDSL._

import org.bson.types.ObjectId

import org.joda.time._

import com.anchortab.actor._
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
    case "recent-billing-history" :: Nil => recentBillingHistory
  }

  def recentBillingHistory = {
    {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
        customerId <- user.stripeCustomerId
        invoices <- tryo(stripe.Invoice.all(Map("customer" -> customerId)))
      } yield {
        ClearClearable andThen
        ".no-invoices" #> ClearNodes &
        ".invoice" #> invoices.data.map { invoice=>
          ".date *" #> new DateTime(invoice.date * 1000).toString("yyyy-MM-dd HH:mm:ss z") &
          ".amount *" #> ("$%.2f" format invoice.total/100d) &
          ".details-link [href]" #> "#"
        }
      }
    } openOr {
      ClearClearable andThen
      ".invoice" #> ClearNodes
    }
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
      ".trial-plan" #> ClearNodes &
      ".cancelled-subscription" #> ClearNodes &
      ".provisional-plan" #> ClearNodes &
      ".special-plan-assignment" #> ClearNodes &
      ".subscribed" #> ClearNodes
    }
  }

  def planSelection = {
    def changeSubscription(user: User, subscription: Option[UserSubscription], newPlanId: ObjectId)() = {
      def maybeCancelSubscription(customer: stripe.Customer) = {
        customer.subscriptions.data match {
          case Nil => Full(false)
          case singleSubscription :: Nil =>
            tryo(customer.cancelSubscription(singleSubscription.id))
          case multipleSubscriptions =>
            logger.error("The customer " + customer.id + " has multiple subscriptions. Bailing.")
            Failure("There are somehow multiple subscriptions on your Stripe account. Please contact us.")
        }
      }

      val changeResult: Box[JsCmd] =
        if (user.activeCard.isDefined) {
          for {
            plan <- (Plan.find(newPlanId):Box[Plan]) ?~ "Plan not found."
            newPlanStripeId <- (plan.stripeId:Box[String]) ?~ "Plan lacks Stripe ID."
            customerId <- (user.stripeCustomerId:Box[String]) ?~ "User lacks Stripe ID."
            customer <- tryo(stripe.Customer.retrieve(customerId)) ?~ "Stripe doesn't recognize user."
            cancelledStripeSubscription <- maybeCancelSubscription(customer)
            updatedStripeSubscription <- tryo(customer.createSubscription(Map(
              "plan" -> newPlanStripeId
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

            if (Props.productionMode)
              EmailActor ! SendAdminNotificationEmail(PlanChanged(user.plan._id, plan._id), user.email)

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
          existingStripeSubscription <- customer.subscriptions.data.headOption
          stripeSubscription <- tryo(customer.cancelSubscription(
            existingStripeSubscription.id,
            Map(
            "at_period_end" -> true
            ))
          )
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
        val currentSubscriptionCanceling = subscription.filter(_.plan.isDefined).map(_.cancelled_?).getOrElse(false)

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
          cardId <- (updatedCustomer.defaultCard: Box[String]) ?~! "Could not find active card id after update."
          card <- (updatedCustomer.cards.data.find(_.id == cardId): Box[stripe.Card]) ?~! "Could not find card in cards list after update."
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
