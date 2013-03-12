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
          eventType match {
            case "invoice.payment_succeeded" =>
              // Send an email reciept.

            case "invoice.payment_failed" =>
              // Send a charge failure alert.

            case "customer.subscription.updated" =>
              // Send updated alert email.
              // Update database to reflect new sub.

            case "customer.subscription.deleted" =>
              // Sorry to see you go email?

            case "customer.subscription.trial_will_end" =>
              // Alert email for trial ending.

            case _ =>
          }

          OkResponse()
        }
      }
  }
}
