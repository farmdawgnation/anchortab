package com.anchortab.migrate

import com.anchortab.model._

import net.liftweb._
  import http._
    import js._
      import JsCmds._
    import SHtml._
    import LiftRules._
  import common._
  import util._
    import Helpers._
  import json._
    import Extraction._
    import JsonDSL._
  import mongodb.BsonDSL._

import org.joda.time._

import org.bson.types.ObjectId

import com.stripe

object GrantBetaUsersTrialSubscription {
  implicit val formats = User.formats

  def run(planId: ObjectId) = {
    for {
      plan <- Plan.find(planId)
      stripePlanId <- plan.stripeId
    } {
      println ("Granting beta users a 30-day trial of " + plan.name)

      val thirtyDaysFromNow = (new DateTime()).plusDays(30).getMillis() / 1000
      println("Thirty days from now is timecode " + thirtyDaysFromNow)

      println("Starting trial grant.")

      for {
        user <- User.findAll
        stripeCustomerId <- user.stripeCustomerId
        customer <- tryo(stripe.Customer.retrieve(stripeCustomerId))
        updatedSubscription <- tryo(customer.updateSubscription(Map(
          "plan" -> stripePlanId,
          "trial_end" -> thirtyDaysFromNow
        )))
      } {
        println("Associating subscription for " + user.email)

        val subscription = UserSubscription(planId, plan.price, plan.term, status = "active")
        User.update("_id" -> user._id, "$addToSet" -> ("subscriptions" -> decompose(subscription)))

      }

      println("Migration complete.")
    }
  }
}
