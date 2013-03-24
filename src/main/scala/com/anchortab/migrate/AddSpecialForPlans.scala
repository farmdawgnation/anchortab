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

object AddSpecialForPlans {
  def run = {
    Plan.update(JObject(Nil), "$set" -> ("isSpecial" -> false), Multi)
  }
}
