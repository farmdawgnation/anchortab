package com.anchortab.actor

import net.liftweb._
  import common._
  import actor._
  import mongodb._
  import util.Helpers._
  import json._
    import ext._
    import JsonDSL._
    import Extraction._

import org.bson.types.ObjectId

import com.anchortab.model.EventLog

case class TrackEvent(eventType:String, ip:String, userAgent:String, userId:ObjectId, tabId:ObjectId,
                      cookieId:Option[String] = None, email:Option[String] = None)

object EventActor extends LiftActor {
  def messageHandler = {
    case TrackEvent(eventType, ip, userAgent, userId, tabId, cookieId, email) =>
      EventLog.track(eventType, ip, userAgent, userId, tabId, cookieId, email)
  }
}
