package com.anchortab.snippet

import scala.xml._

import java.text.SimpleDateFormat
import java.util.Date

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
  import mongodb.BsonDSL._

import com.anchortab.model._

import org.bson.types.ObjectId

import org.joda.time._

case class TabEmbedCodeReceived(embedCode: String) extends SimpleAnchorTabEvent("tab-embed-code-received")
case class NewTabCreated(embedCode: String) extends SimpleAnchorTabEvent("new-tab-created")

object Tabs extends Loggable {
}
