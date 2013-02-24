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

import me.frmr.wepay._
  import api.Preapproval

import com.anchortab.model._

object Subscription extends Loggable {
  implicit val authenticationToken : Option[WePayToken] = {
    for {
      anchortabUserId <- Props.getLong("wepay.anchorTabUserId")
      anchortabAccessToken <- Props.get("wepay.anchorTabAccessToken")
    } yield {
      WePayToken(anchortabUserId, anchortabAccessToken, "BEARER", None)
    }
  }

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
          if (plan.visibleOnRegistration)
            ".special-plan-assignment" #> ClearNodes
          else
            ".subscribed" #> ClearNodes
        }

        provisionalMessage andThen
        planStatus andThen
        ".plan-name *" #> plan.name &
        ".not-subscribed" #> ClearNodes
      }
    } openOr {
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
      def recordCancel(cancelDate: Option[DateTime] = None) = {
        val newSubscription =
          subscription.copy(
            status = "cancelled",
            ends = cancelDate
          )

        val otherSubscriptions = user.subscriptions.filterNot(_._id == newSubscription._id)
        val newUser = user.copy(
          subscriptions = otherSubscriptions ++ (newSubscription :: Nil)
        )
        newUser.save
      }

      {
        if (subscription.price == 0) {
          recordCancel()
          Some(
            Alert("Your subscription is cancelled.") &
            Reload
          )
        } else if (! subscription.lastBilled.isDefined) {
          Some(Alert("Sorry, we can't cancel your subscription until you've been billed for your current one. Email hello@anchortab.com if you believe this is an error."))
        } else {
          for {
            preapprovalId <- subscription.preapprovalId
            cencelResult <- Preapproval.cancel(preapprovalId)
          } yield {
            recordCancel(subscription.lastBilled.map(_.plusMonths(1)))

            Alert("Your subscription is cancelled.") &
            Reload
          }
        }
      } getOrElse {
        Alert("An unexpected error occured while canceling. Email us at hello@anchortab.com to let us know you saw this error.")
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

        ClearClearable andThen
        ".plan" #> plans.map { plan =>
          ".plan-name *" #> plan.registrationTitle &
          ".plan-details *" #> plan.description &
          ".select-plan" #> ((plan._id == currentPlan._id) ? ClearNodes | PassThru) &
          ".cancel-plan" #> ((plan._id == currentPlan._id) ? PassThru | ClearNodes) &
          ".cancel-plan [onclick]" #> ajaxInvoke(cancelClick)
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
        subscription <- user.subscription if subscription.price > 0
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
