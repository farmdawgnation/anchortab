package com.anchortab.snippet

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

  val menus =
    menu ::
    Nil
}
