package com.anchortab.model

import net.liftweb._
  import common._
  import mongodb._
  import util.Helpers._
  import json._
    import ext._
    import JsonDSL._
    import Extraction._

import org.joda.time.DateTime

import org.bson.types.ObjectId

case class Event(eventType:String, ip:String, userAgent:String, userId:ObjectId,
                 tabId:ObjectId, email:Option[String] = None,
                 uniqueEventActorId:Option[ObjectId] = None,
                 createdAt:DateTime = new DateTime, _id:ObjectId = ObjectId.get)
    extends MongoDocument[Event] {
  val meta = Event
}
object Event extends MongoDocumentMeta[Event] {
  override def formats = allFormats ++ JodaTimeSerializers.all

  object Types {
    val TabView = "tab-view"
    val TabSubmit = "tab-submit"
    val TabClose = "tab-close"
    val TabReopen = "tab-reopen"
  }
}

object EventLog {
  def track(eventType:String, ip:String, userAgent:String, userId:ObjectId, tabId:ObjectId, cookieId:Option[String], email:Option[String] = None) {
    Event(eventType, ip, userAgent, userId, tabId, email).save
  }
}
