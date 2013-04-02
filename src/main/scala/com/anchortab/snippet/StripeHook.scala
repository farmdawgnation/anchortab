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
    import JsonDSL._
    import Extraction._
  import mongodb._
    import BsonDSL._

import org.bson.types.ObjectId

import com.anchortab.model._
import com.anchortab.actor._

import org.joda.time._

import com.stripe

object StripeHook extends RestHelper with Loggable {
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

  serve {
    case req @ Req("stripe-hook" :: Nil, _, PostRequest) =>
      {
        for {
          requestBody <- req.body
          requestJson <- tryo(Serialization.read[JValue](new String(requestBody)))
          eventType <- (requestJson \ "type").extractOpt[String]
          dataJson = (requestJson \ "data")
          objectJson = (dataJson \ "object")
        } yield {
          println(pretty(render(requestJson)))

          val result: Box[LiftResponse] = eventType match {
            case "invoice.payment_succeeded" =>
              // Send an email reciept.
              for {
                stripeCustomerId <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
                user <- User.find("stripeCustomerId" -> stripeCustomerId)
                totalAmountInCents <- tryo((objectJson \ "total").extract[Long]) ?~! "No total."
              } yield {
                val amount = totalAmountInCents / 100d

                EmailActor ! SendInvoicePaymentSucceededEmail(user.email, amount)
                OkResponse()
              }

            case "invoice.payment_failed" =>
              for {
                stripeCustomerId <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
                user <- User.find("stripeCustomerId" -> stripeCustomerId)
                totalAmountInCents <- tryo((objectJson \ "total").extract[Long]) ?~! "No total."
              } yield {
                val nextPaymentAttemptSecs: Option[Long] =
                  tryo((objectJson \ "next_payment_attempt").extract[Option[Long]])
                  .flatten
                  .headOption

                val nextPaymentAttempt = nextPaymentAttemptSecs.map { nextPaymentAttemptInSecs =>
                  new DateTime(nextPaymentAttemptInSecs * 1000)
                } filter {
                  _ isAfterNow
                }
                val amount = totalAmountInCents / 100d

                EmailActor ! SendInvoicePaymentFailedEmail(user.email, amount, nextPaymentAttempt)
                OkResponse()
              }

            case "customer.subscription.created" =>
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

            case "customer.subscription.updated" =>
              // Send updated alert email.

              // Update database to reflect new sub.
              for {
                stripeCustomerId <- (objectJson \ "customer").extractOpt[String]
                planJson = (objectJson \ "plan")
                user <- User.find("stripeCustomerId" -> stripeCustomerId)
                currentUserSubscription <- user.subscription
                status <- (objectJson \ "status").extractOpt[String]
                stripePlanId <- (planJson \ "id").extractOpt[String]
                plan <- Plan.find("stripeId" -> stripePlanId)
              } yield {
                implicit val formats = User.formats

                if (plan._id == user.plan._id && currentUserSubscription.status != status) {
                  val newStatus = status match {
                    case "trialing" => "trial"
                    case _ => "active"
                  }

                  User.update(
                    ("_id" -> user._id) ~
                    ("subscriptions._id" -> currentUserSubscription._id),
                    "$set" -> ("subscriptions.$.status" -> newStatus)
                  )
                } else if (plan._id != user.plan._id) {
                  newPlanFromStripe(user, plan, status)
                }

                OkResponse()
              }

            case "customer.subscription.deleted" =>
              // Sorry to see you go email?
              for {
                stripeCustomerId <- (objectJson \ "customer").extractOpt[String]
                user <- User.find("stripeCustomerId" -> stripeCustomerId)
                status <- (objectJson \ "status").extractOpt[String]
                periodEnd <- (objectJson \ "current_period_end").extractOpt[Long]
              } yield {
                implicit val formats = User.formats

                user.subscription match {
                  case Some(subscription) =>
                    if (status == "canceled") {
                      User.update(
                        ("_id" -> user._id) ~
                        ("subscriptions._id" -> subscription._id),
                        "$set" -> (
                          ("subscriptions.$.status" -> "cancelled") ~
                          ("subscriptions.$.ends" -> decompose(new DateTime(periodEnd * 1000)))
                        )
                      )
                    } else { //unpaid
                      User.update(
                        ("_id" -> user._id) ~
                        ("subscriptions._id" -> subscription._id),
                        (
                          "$set" -> (
                            ("subscriptions.$.status" -> "stopped") ~
                            ("subscriptions.$.ends" -> decompose(new DateTime()))
                          ) ~
                          (
                            "$unset" -> ("activeCard" -> true)
                          )
                        )
                      )
                    }

                  case _ =>
                }

                OkResponse()
              }

            case "customer.subscription.trial_will_end" =>
              for {
                stripeCustomerId <- tryo((objectJson \ "customer").extract[String]) ?~! "No customer."
                trialEndInSecs <- tryo((objectJson \ "trial_end").extract[Long]) ?~! "No trial end."
                user <- User.find("stripeCustomerId" -> stripeCustomerId)
                subscription <- user.subscription
                plan <- subscription.plan
              } yield {
                val trialEnd = new DateTime(trialEndInSecs * 1000)

                // We get a trial end event every time a user switches plans. So, we need to distinguish
                // between those and the real ones that come in advance of a real trial ending. We use
                // the heuristic of subtracting one hour from the time to see if it's still in the future.
                if (trialEnd.minusHours(1) isAfterNow)
                  EmailActor ! SendTrialEndingEmail(user.email, user.activeCard.isDefined, plan.name, trialEnd)

                OkResponse()
              }

            case _ =>
              Full(OkResponse())
          }

          result match {
            case Full(resp) => resp

            case Empty => NotFoundResponse()

            case Failure(msg, _, _) =>
              PlainTextResponse(msg, Nil, 500)
          }
        }
      }
  }
}
