package com.anchortab.snippet

import scala.xml.NodeSeq

import net.liftweb._
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

object Subscription extends Loggable {
  implicit val formats = DefaultFormats

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
        val provisionalMessage = {
          if (subscription.provisional_?)
            ".provisional-plan" #> PassThru &
            ".provisional-expiration *" #> subscription.provisionalGracePeriodEnd.toString()
          else
            ".provisional-plan" #> ClearNodes
        }

        val planStatus = {
          if (plan.visibleOnRegistration && ! subscription.cancelled_?)
            ".special-plan-assignment" #> ClearNodes andThen
            ".cancelled-subscription" #> ClearNodes
          else if (subscription.cancelled_?)
            ".subscribed" #> ClearNodes andThen
            ".special-plan-assignment" #> ClearNodes
          else
            ".subscribed" #> ClearNodes andThen
            ".cancelled-subscription" #> ClearNodes
        }

        provisionalMessage andThen
        planStatus andThen
        ".plan-name *" #> plan.name &
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
    def changeSubscription(user: User, subscription: UserSubscription, newPlanId: ObjectId) = {
      // TODO
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

          Reload
        }
      } getOrElse {
        Alert("Error while canceling subscription. Please contact us.")
      }
    }

    {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
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

        val plans = Plan.findAll(("visibleOnRegistration" -> true))

        if (! subscription.map(_.cancelled_?).getOrElse(false)) {
          ClearClearable andThen
          ".plan" #> plans.map { plan =>
            ".plan-name *" #> plan.registrationTitle &
            ".ending-date *" #> subscription.flatMap(_.ends.map(_.toString())).getOrElse("") &
            ".plan-details *" #> plan.description &
            ".select-plan" #> ((plan._id == currentPlan._id) ? ClearNodes | PassThru) &
            ".cancel-plan" #> ((plan._id == currentPlan._id) ? PassThru | ClearNodes) &
            ".cancel-plan [onclick]" #> ajaxInvoke(cancelClick)
          }
        } else {
          ClearNodes
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
          session <- userSession.is
          user <- User.find(session.userId)
          customerId <- user.stripeCustomerId
          customer <- tryo(stripe.Customer.retrieve(customerId))
          updatedCustomer <- tryo(customer.update(Map("card" -> token)))
          card <- updatedCustomer.activeCard
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
          Alert("An internal error occured. Please contact us and let us know.")

        case Failure(msg, _, _) =>
          Alert("An error occured while updating billing information: " + msg)

        case _ =>
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
