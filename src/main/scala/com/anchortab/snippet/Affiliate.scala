package com.anchortab.snippet

import scala.xml._

import net.liftweb._
  import sitemap._
    import Loc._
  import http._
    import js._
      import JsCmds._
    import SHtml._
    import LiftRules._
  import common._
  import util._
    import Helpers._
  import json._
    import JsonDSL._
  import mongodb.BsonDSL._

import com.anchortab.model._

object Affiliate extends Loggable {
  val menu = Menu.i("Affiliate Info") / "manager" / "affiliate"
  val referralMenu = Menu.param[String]("Referral", Text("Referral"), Full(_), _.toString) /
    "referral" / *

  val menus =
    menu ::
    Nil

  def snippetHandlers: SnippetPF = {
    case "referral-url" :: Nil => referralUrl
  }

  def referralUrl = {
    val url = {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
        code <- user.affiliateCode
      } yield {
        "http://" + S.hostName + referralMenu.toLoc.calcHref(code)
      }
    }

    ".referral-url *" #> url
  }
}
