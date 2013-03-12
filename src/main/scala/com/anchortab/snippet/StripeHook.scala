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
  serve {
    case req @ Req("stripe-hook" :: Nil, _, PostRequest) =>
      {
        for {
          requestBody <- req.body
          requestJson <- tryo(Serialization.read[JValue](new String(requestBody)))
          eventType <- (requestJson \ "type").extractOpt[String]
        } yield {
          val result: Box[LiftResponse] = eventType match {
            case "invoice.payment_succeeded" =>
              // Send an email reciept.
              Full(OkResponse())

            case "invoice.payment_failed" =>
              // Send a charge failure alert.
              Full(OkResponse())

            case "customer.subscription.created" =>
              // Create the subscription if it doesn't already exist for that user
              // on our end.
              for {
                stripeCustomerId <- (requestJson \ "customer").extractOpt[String]
                planJson = (requestJson \ "plan")
                status = (requestJson \ "status").extractOpt[String]
                stripePlanId <- (planJson \ "id").extractOpt[String]
                user <- User.find("stripeCustomerId" -> stripeCustomerId)
                plan <- Plan.find("stripeId" -> stripePlanId)
              } yield {
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

                OkResponse()
              }

            case "customer.subscription.updated" =>
              // Send updated alert email.

              // Update database to reflect new sub.
              for {
                stripeCustomerId <- (requestJson \ "customer").extractOpt[String]
                planJson = (requestJson \ "plan")
                user <- User.find("stripeCustomerId" -> stripeCustomerId)
                currentUserSubscription <- user.subscription
                status <- (requestJson \ "status").extractOpt[String]
                stripePlanId <- (planJson \ "id").extractOpt[String]
              } yield {
                implicit val formats = User.formats

                if (currentUserSubscription.status == "trial" && status == "active") {
                  User.update(
                    ("_id" -> user._id) ~
                    ("subscriptions._id" -> currentUserSubscription._id),
                    "$set" -> ("subscriptions.$.status" -> "active")
                  )
                }

                OkResponse()
              }

            case "customer.subscription.deleted" =>
              // Sorry to see you go email?
              for {
                stripeCustomerId <- (requestJson \ "customer").extractOpt[String]
                user <- User.find("stripeCustomerId" -> stripeCustomerId)
                status <- (requestJson \ "status").extractOpt[String]
                periodEnd <- (requestJson \ "current_period_end").extractOpt[Long]
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
              // Alert email for trial ending.
              Full(OkResponse())

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
