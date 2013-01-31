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

import com.ecwid.mailchimp._
  import method.helper._

object RetrieveMailchimpUsernames {
  implicit val formats = DefaultFormats

  def run = {
    println("Retrieving mailchimp usernames for users with mailchimp credentials.")

    val mcClient = new MailChimpClient
    val gadMethod = new GetAccountDetailsMethod
    gadMethod.exclude = "modules" :: "orders" :: "rewards-credits" ::
      "rewards-inspections" :: "rewards-referrals" :: "rewards-applied" :: Nil

    for {
      user <- User.findAll
      credentials <- user.credentialsFor("Mailchimp")
      token <- credentials.serviceCredentials.get("token")
    } {
      gadMethod.apikey = token

      val accountDetails = tryo(mcClient.execute(gadMethod))

      for (details <- accountDetails) {
        val serviceCredential = UserServiceCredentials("Mailchimp", details.username, Map("token" -> token))

        User.update("_id" -> user._id, "$pull" ->
          ("serviceCredentials" -> ("serviceName" -> "Mailchimp"))
        )

        User.update("_id" -> user._id, "$addToSet" ->(
          ("serviceCredentials" -> decompose(serviceCredential))
        ))
      }
    }
  }
}
