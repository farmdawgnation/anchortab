package com.anchortab.snippet

import net.liftweb._
  import common._
  import http._
    import LiftRules._
    import rest._
  import util._
    import Helpers._
  import json._
    import JsonDSL._
  import mongodb.BsonDSL._

import com.anchortab.model._

object Api extends RestHelper with Loggable {
  serve {
    //////////
    // API "embed" resource.
    //
    // Calls in this namespace are used by the Tab itself to retrieve the information
    // it needs to render, or to add an email address to the Tab's email list.
    //////////
    case Req("api" :: "v1" :: "embed" :: tabId :: Nil, _, GetRequest) =>
      {
        for {
          tab <- (Tab.find(tabId):Box[Tab])
          user <- tab.user.filter(_.tabsActive_?) ?~! "This tab has been disabled." ~> 403
        } yield {
          ("delay" -> tab.appearance.delay) ~
          ("font" -> tab.appearance.font) ~
          ("colorScheme" -> tab.appearance.colorScheme) ~
          ("customText" -> tab.appearance.customText)
        }
      } ?~! "Unknown Tab." ~> 404

    case req @ Req("api" :: "v1" :: "embed" :: tabId :: Nil, _, PutRequest) =>
      {
        for {
          tab <- (Tab.find(tabId):Box[Tab]) // Force box type.
          user <- tab.user.filter(_.tabsActive_?) ?~! "This tab has been disabled." ~> 403
          email <- req.param("email") ?~! "Email was not specified." ~> 403
        } yield {
          // FIXME: Actually record the email submission.
          ("success" -> 1) ~
          ("email" -> email)
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
          currentUser <- statelessUser.is if currentUser.admin_?
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
          delay <- req.param("delay").map(_ toInt) ?~! "Delay parameter is required." ~> 400
          font <- req.param("font") ?~! "The font parameter is required." ~> 400
          colorScheme <- req.param("colorScheme") ?~! "The colorScheme parameter is required." ~> 400
          customText <- req.param("customText") ?~! "the customText parameter is required." ~> 400
        } yield {
          Tab.update("_id" -> tabId, "$set" -> (
            "appearance" -> (
              ("delay" -> delay) ~
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
