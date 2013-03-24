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

import com.stripe

object CreateStripeCustomersForUsers {
  def run = {
    println("Creating stripe customers for users.")

    for {
      user <- User.findAll
        if ! user.stripeCustomerId.isDefined
      customer <- tryo(stripe.Customer.create(Map(
        "email" -> user.email
      )))
    } {
      User.update("_id" -> user._id, "$set" -> ("stripeCustomerId" -> customer.id))
    }
  }
}
