package com.anchortab.model

import net.liftweb._
  import common._
  import mongodb._

import org.bson.types.ObjectId

case class TabAppearance(delay:Int, font:String, colorScheme:String, customText:String)

case class TabService(serviceId:String, credentials:Map[String,String])

case class Tab(name:String, userId:ObjectId, appearance:TabAppearance, service:Option[TabService] = None,
               _id:ObjectId = ObjectId.get) extends MongoDocument[Tab] {
  val meta = Tab

  lazy val user : Box[User] = User.find(userId)
}

object Tab extends MongoDocumentMeta[Tab]