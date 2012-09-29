package com.anchortab.model

import net.liftweb._
  import common._
  import mongodb._
  import json._
    import JsonDSL._
    import Extraction._

import org.bson.types.ObjectId

case class TabAppearance(delay:Int, font:String, colorScheme:String, customText:String)

case class TabService(serviceId:String, credentials:Map[String,String])

case class Tab(name:String, userId:ObjectId, appearance:TabAppearance, service:Option[TabService] = None,
               _id:ObjectId = ObjectId.get) extends MongoDocument[Tab] {
  val meta = Tab
  implicit val formats = DefaultFormats

  lazy val user : Box[User] = User.find(userId)

  lazy val asJson =
    ("name" -> name) ~
    ("appearance" -> decompose(appearance)) ~
    ("service" -> decompose(service))
}

object Tab extends MongoDocumentMeta[Tab]
