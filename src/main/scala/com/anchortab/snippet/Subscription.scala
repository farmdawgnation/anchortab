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
  import util._
    import Helpers._

import org.bson.types.ObjectId

import org.joda.time._

import com.anchortab.model._

object Subscription extends Loggable {
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
            ends = Some(new DateTime(periodEnd))
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
    {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
        subscription <- user.subscription
          if subscription.price > 0 && ! subscription.cancelled_?
      } yield {
        if (subscription.active_?) {
          ".need-billing-information" #> ClearNodes
        } else {
          ".billing-information-on-file" #> ClearNodes
        }
      }
    } openOr {
      ".billing-information" #> ClearNodes
    }
  }
}
