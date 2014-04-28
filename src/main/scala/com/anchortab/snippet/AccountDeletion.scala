package com.anchortab.snippet

import net.liftweb.common._
import net.liftweb.util._
  import Helpers._
import net.liftweb.mongodb.BsonDSL._

import com.anchortab.model._
import com.stripe

trait AccountDeletion extends Loggable {
  def deleteStripeCustomer(stripeCustomerId: Box[String]) = {
    for (stripeCustomerId <- stripeCustomerId) {
      tryo(stripe.Customer.retrieve(stripeCustomerId).delete) match {
        case error: EmptyBox =>
          logger.error("Error encountered while deleting Stripe customer: " + error)

        case _ =>
      }
    }
  }

  def deleteAccount(user: User) = {
    for {
      tabDelete <- tryo(Tab.delete("userId" -> user._id)) // Nuke tabs.
      _ = deleteStripeCustomer(user.stripeCustomerId)
      userDelete <- tryo(user.delete)
    } yield {
      true
    }
  }
}
