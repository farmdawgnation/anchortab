package com.anchortab.snippet

import net.liftweb._
  import common._
  import http._
    import LiftRules._
    import rest._
    import js._
      import JE._
      import JsExp._
  import util._
    import Helpers._
  import json._
    import Extraction._
  import mongodb._
    import BsonDSL._

import org.bson.types.ObjectId

import com.anchortab.model._
import com.anchortab.actor._

import org.joda.time._

import com.stripe

object StripeHook extends StripeHook {
  override val emailActor = EmailActor
}
trait StripeHook extends RestHelper with Loggable {
  def emailActor: EmailActor

  private def newPlanFromStripe(user: User, plan: Plan, status: String) = {
    implicit val formats = User.formats

    val newPlanStatus = {
      if (status == "trialing")
        "trial"
      else
        "active"
    }
    val newSub = UserSubscription(plan._id, plan.price, plan.term, status = newPlanStatus)

    user.subscription match {
      case Some(sub) =>
        User.update(
          ("_id" -> user._id) ~
          ("subscriptions._id" -> sub._id),
          "$set" -> (
            ("subscriptions.$.status" -> "cancelled")
          )
        )

      case _ =>
    }

    User.update("_id" -> user._id, "$addToSet" -> ("subscriptions" -> decompose(newSub)))
  }

  def invoicePaymentSucceeded(objectJson: JValue) = {
    for {
      stripeCustomerId <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
      user <- User.find("stripeCustomerId" -> stripeCustomerId)
      invoiceId <- tryo((objectJson \ "id").extract[String]) ?~! "No ID."
      invoice <- tryo(stripe.Invoice.retrieve(invoiceId))
    } yield {
      emailActor ! SendInvoicePaymentSucceededEmail(user, invoice)
      OkResponse()
    }
  }

  def invoicePaymentFailed(objectJson: JValue) = {
    for {
      stripeCustomerId <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
      user <- User.find("stripeCustomerId" -> stripeCustomerId)
      totalAmountInCents <- tryo((objectJson \ "total").extract[Long]) ?~! "No total."
    } yield {
      if (user.activeCard.isDefined) {
        val nextPaymentAttemptSecs: Option[Long] =
          tryo((objectJson \ "next_payment_attempt").extract[Option[Long]]).openOr(None)

        val nextPaymentAttempt = nextPaymentAttemptSecs.map { nextPaymentAttemptInSecs =>
          new DateTime(nextPaymentAttemptInSecs * 1000)
        } filter {
          _ isAfterNow
        }
        val amount = totalAmountInCents / 100d

        emailActor ! SendInvoicePaymentFailedEmail(user.email, amount, nextPaymentAttempt)
      }

      OkResponse()
    }
  }

  def customerSubscriptionCreated(objectJson: JValue) = {
    // Create the subscription if it doesn't already exist for that user
    // on our end.
    for {
      stripeCustomerId <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
      planJson = (objectJson \ "plan")
      status <- (objectJson \ "status").extractOpt[String]
      stripePlanId <- (planJson \ "id").extractOpt[String]
      user <- User.find("stripeCustomerId" -> stripeCustomerId)
      plan <- Plan.find("stripeId" -> stripePlanId)
    } yield {
      newPlanFromStripe(user, plan, status)

      OkResponse()
    }
  }

  def customerSubscriptionUpdated(objectJson: JValue) = {
    // Update database to reflect new sub.
    for {
      stripeCustomerId <- (objectJson \ "customer").extractOpt[String]
      planJson = (objectJson \ "plan")
      user <- User.find("stripeCustomerId" -> stripeCustomerId)
      currentUserSubscription <- user.subscription
      status <- (objectJson \ "status").extractOpt[String]
      stripePlanId <- (planJson \ "id").extractOpt[String]
      cancelAtPeriodEnd <- (objectJson \ "cancel_at_period_end").extractOpt[Boolean]
      plan <- Plan.find("stripeId" -> stripePlanId)
    } yield {
      implicit val formats = User.formats

      if (
        plan._id == user.plan._id && (
          currentUserSubscription.status != status ||
          cancelAtPeriodEnd
        )
      ) {
        val (setPlanEnding: JObject, unsetPlanEnding: JObject) = {
          if (cancelAtPeriodEnd) {
            val currentPeriodEnd = (objectJson \ "current_period_end").extractOpt[Long]
            val planEndingDate = currentPeriodEnd.map(_ * 1000).map(new DateTime(_))

            (("subscriptions.$.ends" -> decompose(planEndingDate)):JObject, JObject(Nil))
          } else if (status == "canceled") {
            val endedAt = (objectJson \ "ended_at").extractOpt[Long]
            val planEndingDate = endedAt.map(_ * 1000).map(new DateTime(_))

            (("subscriptions.$.ends" -> decompose(planEndingDate)):JObject, JObject(Nil))
          } else {
            (JObject(Nil), ("subscriptions.$.ends" -> true):JObject)
          }
        }

        val newStatus = status match {
          case "trialing" => "trial"
          case "canceled" => "cancelled"
          case _ if cancelAtPeriodEnd => "cancelled"
          case _ => "active"
        }

        User.update(
          ("_id" -> user._id) ~
          ("subscriptions._id" -> currentUserSubscription._id),
          ("$set" ->
            (
              ("subscriptions.$.status" -> newStatus) ~
              setPlanEnding
            )
          ) ~
          ("$unset" -> unsetPlanEnding)
        )
      } else if (plan._id != user.plan._id) {
        newPlanFromStripe(user, plan, status)

        if (Props.productionMode)
          EmailActor ! SendAdminNotificationEmail(PlanChanged(user.plan._id, plan._id), user.email)
      }

      OkResponse()
    }
  }

  def customerSubscriptionDeleted(objectJson: JValue) = {
    // Sorry to see you go email?
    for {
      stripeCustomerId <- (objectJson \ "customer").extractOpt[String]
      user <- User.find("stripeCustomerId" -> stripeCustomerId)
      status <- (objectJson \ "status").extractOpt[String]
      periodEnd <- (objectJson \ "current_period_end").extractOpt[Long]
      cancelAtPeriodEnd <- (objectJson \ "cancel_at_period_end").extractOpt[Boolean]
    } yield {
      implicit val formats = User.formats

      user.subscription match {
        case Some(subscription) =>
          val (subscriptionStatus, subscriptionEnds) = if (cancelAtPeriodEnd) {
            ("cancelled", decompose(new DateTime(periodEnd * 1000)))
          } else {
            ("stopped", decompose(new DateTime()))
          }

          User.update(
            ("_id" -> user._id) ~
            ("subscriptions._id" -> subscription._id),
            "$set" -> (
              ("subscriptions.$.status" -> subscriptionStatus) ~
              ("subscriptions.$.ends" -> subscriptionEnds)
            )
          )

        case _ =>
      }

      if (Props.productionMode)
        EmailActor ! SendAdminNotificationEmail(SubscriptionDeleted, user.email)

      OkResponse()
    }
  }

  private def validStripeEventId(id: String): Option[String] = {
    RecordedStripeEvent.find("stripeEventId" -> id) match {
      case None => Some(id)
      case _ => None
    }
  }

  serve {
    case req @ Req("stripe-hook" :: Nil, _, PostRequest) =>
      {
        for {
          requestBody <- req.body
          requestJson <- tryo(Serialization.read[JValue](new String(requestBody)))
          id <- (requestJson \ "id").extractOpt[String]
          validStripeId <- validStripeEventId(id)
          eventType <- (requestJson \ "type").extractOpt[String]
          dataJson = (requestJson \ "data")
          objectJson = (dataJson \ "object")
        } yield {
          val result: Box[LiftResponse] = eventType match {
            case "invoice.payment_succeeded" => invoicePaymentSucceeded(objectJson)
            case "invoice.payment_failed" => invoicePaymentFailed(objectJson)
            case "customer.subscription.created" => customerSubscriptionCreated(objectJson)
            case "customer.subscription.updated" => customerSubscriptionUpdated(objectJson)
            case "customer.subscription.deleted" => customerSubscriptionDeleted(objectJson)
            case _ => Full(OkResponse())
          }

          result match {
            case Full(resp) if resp.isInstanceOf[OkResponse] =>
              RecordedStripeEvent(validStripeId).save
              resp

            case Full(resp) => resp

            case Empty => NotFoundResponse()

            case Failure(msg, _, _) =>
              PlainTextResponse(msg, Nil, 500)
          }
        }
      }
  }
}
