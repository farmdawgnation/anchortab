package com.anchortab.snippet

import scala.xml.NodeSeq

import net.liftweb._
  import common._
  import http._
    import provider._
    import js._
      import JsCmds._
    import SHtml._
    import LiftRules._
  import util._
    import Helpers._
  import json._
    import JsonDSL._
  import mongodb.BsonDSL._

object inviteCode extends RequestVar[Box[String]](Empty)

object Invites extends Loggable {
  def statelessRewrite: RewritePF = {
    case RewriteRequest(ParsePath("accept-invite" :: userInviteCode :: Nil, _, _, _), _, _) =>
      inviteCode(Full(userInviteCode))
      RewriteResponse("register" :: Nil)
  }
}
