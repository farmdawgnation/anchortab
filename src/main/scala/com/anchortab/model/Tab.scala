package com.anchortab.model

import net.liftweb._
  import common._
  import mongodb._
  import json._
    import ext._
    import JsonDSL._
    import Extraction._
  import util.Helpers._

import org.bson.types.ObjectId

case class TabAppearance(delay:Int, font:String, colorScheme:String, customText:String)
object TabAppearance {
  val defaults = TabAppearance(30, "Arial", "red", "")
}

case class TabService(serviceId:String, credentials:Map[String,String])

case class TabStats(views:Long = 0, submissions:Long = 0)

case class Tab(name:String, userId:ObjectId, appearance:TabAppearance, service:Option[TabService] = None,
               stats:TabStats = new TabStats, _id:ObjectId = ObjectId.get) extends MongoDocument[Tab] {
  val meta = Tab

  lazy val user : Box[User] = User.find(userId)

  lazy val asJson = {
    implicit val formats = DefaultFormats

    ("name" -> name) ~
    ("appearance" -> decompose(appearance)) ~
    ("service" -> decompose(service))
  }
}

object Tab extends MongoDocumentMeta[Tab] {
  override def formats = allFormats ++ JodaTimeSerializers.all

  object AppearanceDelayOptions extends Enumeration {
    val Delay0 = Value("0")
    val Delay2 = Value("2")
    val Delay10 = Value("10")
    val Delay30 = Value("30")
    val Delay60 = Value("60")
  }

  object FontOptions extends Enumeration {
    val Arial = Value("Arial")
    val TimesNewRoman = Value("Times New Roman")
    val Helvetica = Value("Helvetica")
  }

  object ColorSchemeOptions extends Enumeration {
    val Red = Value("Red")
    val Blue = Value("Blue")
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
  def validFont(font:String) = validParameter(FontOptions, font)
  def validColorScheme(colorScheme:String) = validParameter(ColorSchemeOptions, colorScheme)
}
