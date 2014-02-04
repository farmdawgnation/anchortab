package com.anchortab.model

import net.liftweb._
  import common._
  import mongodb._
  import json._
    import ext._
    import JsonDSL._
    import Extraction._
  import util._
    import Helpers._

import org.joda.time._

import org.bson.types.ObjectId

case class TabColorScheme(baseColor: String, secondaryColor: String, name: String = "") {
  override val toString = name
}
object TabColorScheme {
  val Gray = TabColorScheme("#848484", "#5e5e5e", "Gray")
  val Green = TabColorScheme("#39bd42", "#2b8a31", "Green")
  val Red = TabColorScheme("#db2f2d", "#9b2220", "Red")
  val Blue = TabColorScheme("#2d8bd6", "#226aa3", "Blue")

  val Custom = TabColorScheme("", "", "Custom")

  val basic = Red :: Green :: Blue :: Gray :: Nil
  val advanced = basic ++ (Custom :: Nil)
}

case class TabAppearance(delay:Int, colorScheme:TabColorScheme, customText:String, whitelabel:Boolean, collectName: Boolean)
object TabAppearance {
  val defaults = TabAppearance(30, TabColorScheme.Red, "", false, false)
}

case class TabStats(views:Long = 0, submissions:Long = 0)

case class TabSubscriber(email:String, name:Option[String] = None, verified:Boolean = false, createdAt:DateTime = new DateTime(),
                         _id:ObjectId = ObjectId.get)

case class Tab(name:String, userId:ObjectId, appearance:TabAppearance, service:Option[ServiceWrapper] = Empty,
               stats:TabStats = new TabStats, subscribers:List[TabSubscriber] = List(),
               _id:ObjectId = ObjectId.get) extends MongoDocument[Tab] {
  val meta = Tab

  lazy val user : Box[User] = User.find(userId)

  lazy val asJson = {
    implicit val formats = DefaultFormats

    ("name" -> name) ~
    ("appearance" -> decompose(appearance)) ~
    ("service" -> decompose(service))
  }

  lazy val subscriberEmails = subscribers.map(_.email)
  def hasSubscriber_?(email:String) = subscriberEmails.contains(email)

  val embedCode =
    """<script id="anchortab-loader" type="text/javascript" data-tab-id="""" + _id + """" src="""" + Tab.embedScriptUrl + """"></script>"""
}

object Tab extends MongoDocumentMeta[Tab] {
  override def formats = (allFormats ++ JodaTimeSerializers.all) +
    ServiceWrapper.typeHints

  lazy val embedScriptUrl = Props.get("anchortab.embedscript") openOr "http://local.anchortab.com/javascripts/load.js"

  object AppearanceDelayOptions extends Enumeration {
    val Delay0 = Value("0")
    val Delay2 = Value("2")
    val Delay10 = Value("10")
    val Delay30 = Value("30")
    val Delay60 = Value("60")
  }

  object EmailServices extends Enumeration {
    val None = Value("None")
    val MailChimp = Value("MailChimp")
    val ConstantContact = Value("Constant Contact")
    val CampaignMonitor = Value("Campaign Monitor")
    val LeadGeneration = Value("Lead Generation")
    val Pardot = Value("Pardot")
  }

  private def validParameter(options:Enumeration, value:String) = {
    tryo(options.withName(value)) match {
      case Full(_) =>
        Full(value)
      case _ =>
        Empty
    }
  }

  def validAppearanceDelay(delay:String) = validParameter(AppearanceDelayOptions, delay)
}
