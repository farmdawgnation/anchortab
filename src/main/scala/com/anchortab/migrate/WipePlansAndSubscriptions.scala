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
  import mongodb._
    import BsonDSL._

object WipePlansAndSubscriptions {
  def run = {
    Plan.drop
    User.update(JObject(Nil), "$set" -> ("subscriptions" -> JArray(List())), Multi)
  }
}
