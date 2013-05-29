package com.anchortab.snippet

import scala.math._

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

import com.newrelic.api.agent._

object Api extends RestHelper with Loggable {
  def statelessRewrite : RewritePF = {
    case RewriteRequest(ParsePath("api" :: "v1" :: "user" :: userId :: "tabs" :: Nil, _, _, _), _, _) =>
      RewriteResponse("api" :: "v1" :: "user" :: userId :: "tabs" :: "0" :: Nil)
  }

  serve {
    //////////
    // API "embed" resource.
    //
    // Calls in this namespace are used by the Tab itself to retrieve the information
    // it needs to render, or to add an email address to the Tab's email list.
    //////////
    case req @ Req("api" :: "v1" :: "embed" :: tabId :: Nil, _, GetRequest) =>
      {
        NewRelic.setTransactionName("API", "/api/v1/embed")
        NewRelic.addCustomParameter("tabId", tabId)

        for {
          tab <- (Tab.find(tabId):Box[Tab]) ?~ "Unknown tab." ~> 404
          user <- tab.user.filter(_.tabsActive_?) ?~ "This tab has been disabled." ~> 403
          subscription <- (user.subscription: Box[UserSubscription]) ?~ "No subscription found." ~> 500
          plan <- (subscription.plan: Box[Plan]) ?~ "No plan found." ~> 500
          callbackFnName <- req.param("callback") ?~ "Callback not specified." ~> 400
        } yield {
          val remoteIp = req.header("X-Forwarded-For") openOr req.remoteAddr
          val userAgent = req.userAgent openOr "unknown"

          Tab.update("_id" -> tab._id, "$inc" -> ("stats.views" -> 1))

          if (user.firstSteps.get(UserFirstStep.Keys.EmbedYourTab).isDefined) {
            User.update("_id" -> user._id,
              "$unset" -> (
                ("firstSteps." + UserFirstStep.Keys.EmbedYourTab) -> true
              )
            )
          }

          User.update("_id" -> user._id,
            "$inc" -> (
              ("quotaCounts." + Plan.Quotas.Views) -> 1
            )
          )

          QuotasActor ! CheckQuotaCounts(user._id)
          EventActor ! TrackEvent(Event.Types.TabView, remoteIp, userAgent, user._id, tab._id)

          val whitelabelTab = tab.appearance.whitelabel && plan.hasFeature_?(Plan.Features.WhitelabeledTabs)
          val colorScheme = {
            if (tab.appearance.colorScheme.name == TabColorScheme.Custom.name && ! plan.hasFeature_?(Plan.Features.CustomColorSchemes)) {
              TabColorScheme.Red
            } else {
              tab.appearance.colorScheme
            }
          }

          val tabJson =
            ("delay" -> tab.appearance.delay) ~
            ("colorScheme" -> decompose(colorScheme)) ~
            ("whitelabel" -> whitelabelTab) ~
            ("customText" -> tab.appearance.customText)

          Call(callbackFnName, tabJson)
        }
      }

    // JSONP can't be semantic. :(
    case req @ Req("api" :: "v1" :: "embed" :: tabId :: "submit" :: Nil, _, GetRequest) =>
      {
        NewRelic.setTransactionName("API", "/api/v1/embed/star/submit")
        NewRelic.addCustomParameter("tabId", tabId)

        for {
          tab <- (Tab.find(tabId):Box[Tab]) ?~ "Unknown tab." ~> 404
          user <- tab.user.filter(_.tabsActive_?) ?~! "This tab has been disabled." ~> 403
          callbackFnName <- req.param("callback") ?~! "Callback not specified." ~> 400
          email <- req.param("email").filter(_.trim.nonEmpty) ?~! "Email was not specified." ~> 400
        } yield {
          val remoteIp = req.header("X-Forwarded-For") openOr req.remoteAddr
          val userAgent = req.userAgent openOr "unknown"

          // Ensure this new subscriber is unique.
          val submitResult =
            if (! tab.hasSubscriber_?(email)) {
              implicit val formats = Tab.formats

              val subscriberInformation = TabSubscriber(email)

              User.update("_id" -> user._id,
                "$inc" -> (
                  ("quotaCounts." + Plan.Quotas.EmailSubscriptions) -> 1
                )
              )

              Tab.update("_id" -> tab._id, (
                ("$inc" -> ("stats.submissions" -> 1)) ~
                ("$addToSet" -> ("subscribers" -> decompose(subscriberInformation)))
              ))

              val successMessage = {
                for {
                  serviceWrapper <- tab.service
                } yield {
                  ServiceWrapperSubmissionActor ! SubscribeEmailToServiceWrapper(serviceWrapper, email)

                  "Success! An email has been sent to confirm your subscription."
                }
              } getOrElse {
                "Success! You're on the list. Expect to hear from us soon."
              }

              ("success" -> 1) ~
              ("email" -> email) ~
              ("message" -> successMessage)
            } else {
              ("success" -> 0) ~
              ("email" -> email) ~
              ("message" -> "Your email is already subscribed to this list, it seems.")
            }

          QuotasActor ! CheckQuotaCounts(user._id)
          EventActor ! TrackEvent(Event.Types.TabSubmit, remoteIp, userAgent, user._id, tab._id)

          Call(callbackFnName, submitResult)
        }
      }

    //////////
    // API "user" resource.
    //////////
    case Req("api" :: "v1" :: "user" :: Nil, _, GetRequest) =>
      {
        for {
          user <- statelessUser.is
        } yield {
          user.asJson
        }
      } ?~ "Authentication Failed." ~> 401

    //////////
    // API "tab" resource
    //////////
    case Req("api" :: "v1" :: "tab" :: tabId :: Nil, _, GetRequest) =>
      {
        for {
          currentUser <- statelessUser.is ?~ "Authentication Failed." ~> 401
          possibleTab = (Tab.find(tabId):Box[Tab])
          tab <- possibleTab.filter { tabInQuestion =>
            tabInQuestion.userId == currentUser._id || currentUser.admin_?
          } ?~ "Unknown tab." ~> 404
        } yield {
          tab.asJson
        }
      }

    //////////
    // API "admin" resource.
    //////////
    case req @ Req("api" :: "v1" :: "admin" :: "users" :: Nil, _, GetRequest) =>
      {
        for {
          possibleAdminUser <- (statelessUser.is ?~ "Authentication Failed." ~> 401)
          adminUser <- (Full(possibleAdminUser).filter(_.admin_?) ?~ "Not authorized." ~> 403)
        } yield {
          decompose(User.findAll.map(_.asJson))
        }
      }

    case req @ Req("api" :: "v1" :: "admin" :: "users" :: Nil, _, PostRequest) =>
      {
        for {
          possibleAdminUser <- (statelessUser.is ?~ "Authentication Failed." ~> 401)
          adminUser <- (Full(possibleAdminUser).filter(_.admin_?) ?~ "Not authorized." ~> 403)
          requestBody <- req.body ?~ "No request body." ~> 400
          requestJson <- tryo(Serialization.read[JValue](new String(requestBody))) ?~! "Invalid JSON." ~> 400
          email <- tryo(requestJson \ "email").map(_.extract[String]) ?~ "Email is missing" ~> 400
          password <- tryo(requestJson \ "password").map(_.extract[String]) ?~ "Password is missing." ~> 400
        } yield {
          val user = User(email, User.hashPassword(password))
          user.save

          ("id" -> user._id.toString):JObject
        }
      }

    case req @ Req("api" :: "v1" :: "admin" :: "user" :: id :: Nil, _, GetRequest) =>
      {
        for {
          possibleAdminUser <- (statelessUser.is ?~ "Authentication Failed." ~> 401)
          adminUser <- (Full(possibleAdminUser).filter(_.admin_?) ?~ "Not authorized." ~> 403)
          user <- (User.find(id):Box[User]) ?~! "User not found." ~> 404
        } yield {
          user.asJson
        }
      }

    case req @ Req("api" :: "v1" :: "admin" :: "user" :: id :: Nil, _, DeleteRequest) =>
      {
        for {
          possibleAdminUser <- (statelessUser.is ?~ "Authentication Failed." ~> 401)
          adminUser <- (Full(possibleAdminUser).filter(_.admin_?) ?~ "Not authorized." ~> 403)
          user <- (User.find(id):Box[User]) ?~! "User not found." ~> 404
        } yield {
          user.delete

          OkResponse()
        }
      }
  }
}
