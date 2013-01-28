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

object AddMailchimpServiceCredentials {
  implicit val formats = DefaultFormats

  def run = {
    println("Adding Mailchimp Service credentials for users...")

    for {
      user <- User.findAll
      tab <- Tab.findAll("userId" -> user._id)
      service <- tab.service
    } {
      service match {
        case MailChimpServiceWrapper(apiKey, _) =>
          println("Found API key " + apiKey + " for user " +  user.email)

          for {
            latestUser <- User.find(user._id)
              if ! latestUser.credentialsFor("Mailchimp").isDefined
          } {
            println("Adding API key to user service credentials.")
            val serviceCredential = UserServiceCredentials("Mailchimp", apiKey, Map("token" -> apiKey))
            User.update("_id" -> user._id, "$addToSet" ->(
              ("serviceCredentials" -> decompose(serviceCredential))
            ))
          }

        case _ =>
      }
    }
  }
}
