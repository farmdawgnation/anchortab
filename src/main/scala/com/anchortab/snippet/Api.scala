package com.anchortab.snippet

import java.util.Locale

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
    import Extraction._
  import mongodb._
    import BsonDSL._

import org.bson.types.ObjectId

import com.anchortab.model._
import com.anchortab.actor._

import com.newrelic.api.agent._

object Api extends RestHelper with Loggable with AccountDeletion {
  private val localizationCache = new collection.mutable.HashMap[Locale, JObject] with collection.mutable.SynchronizedMap[Locale, JObject]

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
          tab <- (Tab.find(tabId):Box[Tab]) ?~ {
            NewRelic.ignoreTransaction
            "Unknown tab."
          } ~> 404
          user <- tab.user.filter(_.tabsActive_?) ?~ {
            NewRelic.ignoreTransaction
            "This tab has been disabled."
          } ~> 403
          plan = user.plan
          callbackFnName <- req.param("callback") ?~ "Callback not specified." ~> 400
        } yield {
          val remoteIp = req.header("X-Forwarded-For") openOr req.remoteAddr
          val userAgent = req.userAgent openOr "unknown"
          val domain = req.header("X-Embedded-Domain")

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
          EventActor ! TrackEvent(Event(
            Event.Types.TabView,
            Some(remoteIp),
            Some(userAgent),
            Some(user._id),
            Some(tab._id),
            domain = domain
          ))

          val whitelabelTab = tab.appearance.whitelabel && plan.hasFeature_?(Plan.Features.WhitelabeledTabs)
          val colorScheme = {
            if (tab.appearance.colorScheme.name == TabColorScheme.Custom.name && ! plan.hasFeature_?(Plan.Features.CustomColorSchemes)) {
              TabColorScheme.Red
            } else {
              tab.appearance.colorScheme
            }
          }

          val localizedContent = localizationCache.getOrElseUpdate(S.locale, {
            List(
              "tab-firstName",
              "tab-emailAddress",
              "tab-submit",
              "tab-subscribe",
              "tab-minimizeAnchorTab",
              "tab-maximizeAnchorTab",
              "tab-somethingWentWrong",
              "tab-invalidEmail"
            ).map({ localizationKey =>
              (localizationKey -> S.?(localizationKey))
            }).foldLeft(JObject(Nil))(_ ~ _)
          })

          val tabJson =
            ("delay" -> tab.appearance.delay) ~
            ("colorScheme" -> decompose(colorScheme)) ~
            ("whitelabel" -> whitelabelTab) ~
            ("customText" -> tab.appearance.customText) ~
            ("submitButtonText" -> tab.appearance.customSubmitButtonText) ~
            ("mobileTabText" -> tab.appearance.customMobileTabText) ~
            ("collectName" -> tab.appearance.collectName) ~
            ("i18n" -> localizedContent)

          Call(callbackFnName, tabJson)
        }
      }

    // JSONP can't be semantic. :(
    case req @ Req("api" :: "v1" :: "embed" :: tabId :: "submit" :: Nil, _, GetRequest) =>
      {
        NewRelic.setTransactionName("API", "/api/v1/embed/star/submit")
        NewRelic.addCustomParameter("tabId", tabId)

        for {
          tab <- (Tab.find(tabId):Box[Tab]) ?~ {
            NewRelic.ignoreTransaction
            "Unknown tab."
          } ~> 404
          user <- tab.user.filter(_.tabsActive_?) ?~ {
            NewRelic.ignoreTransaction
            "This tab has been disabled."
          } ~> 403
          callbackFnName <- req.param("callback") ?~! "Callback not specified." ~> 400
          email <- req.param("email").filter(_.trim.nonEmpty) ?~! "Email was not specified." ~> 400
          name = req.param("name").map(_.trim).filter(_.nonEmpty)
        } yield {
          val remoteIp = req.header("X-Forwarded-For") openOr req.remoteAddr
          val userAgent = req.userAgent openOr "unknown"
          val domain = req.header("X-Embedded-Domain")

          val submitResult = {
            val successMessage = "tab-successConfirm"
            val iFrameParameters =
              tab.service.iFrameParameters.map(decompose _)

            ("success" -> 1) ~
            ("email" -> email) ~
            ("message" -> S.?(successMessage)) ~
            ("iFrame" -> iFrameParameters)
          }

          QuotasActor ! CheckQuotaCounts(user._id)
          EventActor ! TrackEvent(Event(
            Event.Types.TabSubmit,
            Some(remoteIp),
            Some(userAgent),
            Some(user._id),
            Some(tab._id),
            domain = domain
          ))
          ServiceWrapperSubmissionActor ! SubscribeEmailToServiceWrapper(tab, email, name)

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
          //deleteResult <- deleteAccount(user)
        } yield {
          user.delete
          OkResponse()
        }
      }
  }
}
