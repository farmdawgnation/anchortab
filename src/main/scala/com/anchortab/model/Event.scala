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

case class UniqueEventActor(ip:String, userAgent:String, tabId:ObjectId,
                            cookieId:String = randomString(32),
                            createdAt:DateTime = new DateTime,
                            _id:ObjectId = ObjectId.get)
    extends MongoDocument[UniqueEventActor] {
  val meta = UniqueEventActor
}
object UniqueEventActor extends MongoDocumentMeta[UniqueEventActor] {
  override def formats = allFormats ++ JodaTimeSerializers.all
}

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
    val uniqueEventActor =
      UniqueEventActor.find("cookieId" -> cookieId) match {
        case Some(actor:UniqueEventActor) => Some(actor)
        case None if cookieId.isDefined =>
          // Something funky is up. We had a cookie but no corresponding entry in the DB.
          None
        case _ =>
          val actor = UniqueEventActor(ip, userAgent, tabId)
          actor.save
          Some(actor)
      }

    Event(eventType, ip, userAgent, userId, tabId, email, uniqueEventActor.map(_._id)).save
  }
}
