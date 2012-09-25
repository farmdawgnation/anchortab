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
    case Req("api" :: "user" :: id :: Nil, _, GetRequest) =>
      {
        for {
          currentUser <- statelessUser.is
            if currentUser.role == Some("admin")
          user <- (User.find(id):Box[User]) ?~! "User not found." ~> 404
        } yield {
          ("userId" -> user._id) ~
          ("email" -> user.email)
        }
      } ?~! "Authentication Failed." ~> 401
  }
}
