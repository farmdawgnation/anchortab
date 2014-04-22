package com.anchortab.snippet

import net.liftweb.common._
import net.liftweb.util._
  import Helpers._
import net.liftweb.mongodb.BsonDSL._

import com.anchortab.model._
import com.stripe

trait AccountDeletion {
  def deleteAccount(user: User) = {
    for {
      tabDelete <- tryo(Tab.delete("userId" -> user._id)) // Nuke tabs.
      stripeCustomerId <- (user.stripeCustomerId: Box[String]) ?~! "No stripe ID."
      stripeCustomerDelete <- tryo(stripe.Customer.retrieve(stripeCustomerId).delete) // Delete Stripe customer
      userDelete <- tryo(user.delete)
    } yield {
      true
    }
  }
}
