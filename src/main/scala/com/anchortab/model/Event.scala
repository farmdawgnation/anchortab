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

case class Event(
  eventType:String,
  ip:Option[String] = None,
  userAgent:Option[String] = None,
  userId:Option[ObjectId] = None,
  tabId:Option[ObjectId] = None,
  email:Option[String] = None,
  domain: Option[String] = None,
  service: Option[String] = None,
  message: Option[String] = None,
  createdAt:DateTime = new DateTime,
  _id:ObjectId = ObjectId.get
) extends MongoDocument[Event] {
  val meta = Event
}
object Event extends MongoDocumentMeta[Event] {
  override def formats = allFormats ++ JodaTimeSerializers.all

  object Types {
    val TabView = "tab-view"
    val TabSubmit = "tab-submit"
    val TabClose = "tab-close"
    val TabReopen = "tab-reopen"
    val TabSubmitError = "tab-submit-error"
  }
}
