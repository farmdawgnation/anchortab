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
import com.anchortab.actor.{EventActor, TrackEvent}

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
        for {
          tab <- (Tab.find(tabId):Box[Tab])
          user <- tab.user.filter(_.tabsActive_?) ?~! "This tab has been disabled." ~> 403
          callbackFnName <- req.param("callback") ?~! "Callback not specified." ~> 400
        } yield {
          val remoteIp = req.header("X-Forwarded-For") openOr req.remoteAddr
          val userAgent = req.userAgent openOr "unknown"

          val cookieId =
            S.cookieValue("unique-event-actor") match {
              case Full(uniqueEventActorId) => uniqueEventActorId
              case _ =>
                val uniqueEventActor = UniqueEventActor(remoteIp, userAgent, tab._id)
                // TODO WRITE CONCERN ME
                uniqueEventActor.save
                uniqueEventActor.cookieId
            }

          Tab.update("_id" -> tab._id, "$inc" -> ("stats.views" -> 1))
          EventActor ! TrackEvent(Event.Types.TabView, remoteIp, userAgent, user._id, tab._id, Some(cookieId))

          val tabJson =
            ("delay" -> tab.appearance.delay) ~
            ("font" -> tab.appearance.font) ~
            ("colorScheme" -> tab.appearance.colorScheme) ~
            ("customText" -> tab.appearance.customText)

          Call(callbackFnName, tabJson)
        }
      } ?~! "Unknown Tab." ~> 404

    // JSONP can't be semantic. :(
    case req @ Req("api" :: "v1" :: "embed" :: tabId :: "submit" :: Nil, _, GetRequest) =>
      {
        for {
          tab <- (Tab.find(tabId):Box[Tab]) // Force box type.
          user <- tab.user.filter(_.tabsActive_?) ?~! "This tab has been disabled." ~> 403
          callbackFnName <- req.param("callback") ?~! "Callback not specified." ~> 403
          email <- req.param("email") ?~! "Email was not specified." ~> 403
        } yield {
          // Ensure this new subscriber is unique.
          if (! tab.hasSubscriber_?(email)) {
            implicit val formats = Tab.formats

            val subscriberInformation = TabSubscriber(email)

            Tab.update("_id" -> tab._id, (
              ("$inc" -> ("stats.submissions" -> 1)) ~
              ("$addToSet" -> ("subscribers" -> decompose(subscriberInformation)))
            ))
          }

          val submitResult =
            ("success" -> 1) ~
            ("email" -> email)

          Call(callbackFnName, submitResult)
        }
      } ?~! "Unknwon Tab." ~> 404

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
      } ?~! "Authentication Failed." ~> 401

    case Req("api" :: "v1" :: "user" :: id :: Nil, _, GetRequest) =>
      {
        for {
          currentUser <- statelessUser.is
            if id == currentUser._id || currentUser.admin_?
          user <- (User.find(id):Box[User]) ?~! "User not found." ~> 404
        } yield {
          user.asJson
        }
      } ?~! "Authentication Failed." ~> 401

    case Req("api" :: "v1" :: "user" :: "find" :: email :: Nil, _, GetRequest) =>
      {
        for {
          currentUser <- statelessUser.is if currentUser.admin_?
          user <- (User.find("email" -> email):Box[User]) ?~! "User not found." ~> 404
        } yield {
          user.asJson
        }
      } ?~! "Authentication Failed." ~> 401

    case Req("api" :: "v1" :: "user" :: userId :: "tabs" :: AsInt(page) :: Nil, _, GetRequest) =>
      {
        for {
          currentUser <- statelessUser.is
            if userId == currentUser._id || currentUser.admin_?
        } yield {
          val limit = 20
          val skip = max((page - 1) * limit, 0)
          val userTabs =
            Tab.findAll("userId" -> userId, "createdAt" -> 1, Limit(20), Skip(skip)).map(_.asJson)

          ("tabs" -> userTabs):JObject
        }
      } ?~! "Authentication Failed." ~> 401

    case req @ Req("api" :: "v1" :: "user" :: userId :: "tabs" :: Nil, _, PostRequest) =>
      {
        for {
          currentUser <- statelessUser.is
            if userId == currentUser._id || currentUser.admin_?
          name <- req.param("name")
        } yield {
          val tab = Tab(name, new ObjectId(userId), TabAppearance.defaults)
          tab.save

          ("success" -> 1) ~
          ("tabId" -> tab._id)
        }
      } ?~! "Authentication Failed." ~> 401

    //////////
    // API "tab" resource
    //////////
    case Req("api" :: "v1" :: "tab" :: tabId :: Nil, _, GetRequest) =>
      {
        for {
          currentUser <- statelessUser.is
          tab <- (Tab.find(tabId):Box[Tab])
            if tab.userId == currentUser._id || currentUser.admin_?
        } yield {
          tab.asJson
        }
      } ?~! "Tab not found." ~> 404

    case req @ Req("api" :: "v1" :: "tab" :: tabId :: "appearance" :: Nil, _, PutRequest) =>
      {
        for {
          currentUser <- statelessUser.is
          tab <- (Tab.find(tabId):Box[Tab])
            if tab.userId == currentUser._id || currentUser.admin_?
          delay <- req.param("delay") ?~! "Delay parameter is required." ~> 400
          validDelay <- Tab.validAppearanceDelay(delay) ?~! "The delay parameter was invalid." ~> 400
          font <- req.param("font") ?~! "The font parameter is required." ~> 400
          validFont <- Tab.validFont(font) ?~! "The font parameter was invalid." ~> 400
          colorScheme <- req.param("colorScheme") ?~! "The colorScheme parameter is required." ~> 400
          validColorScheme <- Tab.validColorScheme(colorScheme) ?~! "The colorScheme parameter was invalid." ~> 400
          customText <- req.param("customText") ?~! "the customText parameter is required." ~> 400
        } yield {
          Tab.update("_id" -> tabId, "$set" -> (
            "appearance" -> (
              ("delay" -> delay.toInt) ~
              ("font" -> font) ~
              ("colorScheme" -> colorScheme) ~
              ("customTest" -> customText)
            )
          ))

          JObject(Nil)
        }
      } ?~! "Tab not found." ~> 404
  }
}
