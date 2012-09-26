package com.anchortab.model

import net.liftweb._
  import mongodb._

import org.bson.types.ObjectId

case class TabAppearance(appearanceDelay:Int, font:String, colorScheme:String, customText:String)

case class TabService(serviceId:String, credentials:Map[String,String])

case class Tab(name:String, userId:ObjectId, appearance:TabAppearance, service:Option[TabService] = None,
               _id:ObjectId = ObjectId.get) extends MongoDocument[Tab] {
  val meta = Tab
}

object Tab extends MongoDocumentMeta[Tab]
