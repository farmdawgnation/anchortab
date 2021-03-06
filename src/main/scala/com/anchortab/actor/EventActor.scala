package com.anchortab.actor

import scala.collection.mutable.HashMap

import net.liftweb._
  import http._
  import common._
  import actor._
  import mongodb._
    import BsonDSL._
  import util.Helpers._
  import json._
    import ext._
    import Extraction._

import org.bson.types.ObjectId

import org.joda.time._

import com.anchortab.model._
import com.anchortab.comet._

case class TrackEvent(event: Event)
case class EventActorClientCometRegistered(comet: CometActor)
case class EventActorClientCometDeregistered(cometName: String)
case class EventSummaryRequested(comet: CometActor, userId:ObjectId, eventType: String)

case class EventSummaryEntry(name: String, y: Long)
case class EventSummary(name: String, data: List[EventSummaryEntry])

object EventActor extends LiftActor with Loggable {
  val registeredComets: HashMap[String, CometActor] = new HashMap

  def messageHandler = {
    case TrackEvent(event) =>
      event.save

      for (userId <- event.userId if event.eventType == Event.Types.TabSubmit) {
        val targetComet = "event-summary-comet-" + userId

        for(comet <- registeredComets.get(targetComet)) {
          this ! EventSummaryRequested(comet, userId, event.eventType)
        }
      }

    case EventActorClientCometRegistered(comet) =>
      {
        for(cometName <- comet.name) yield {
          registeredComets += (cometName -> comet)
        }
      } openOr {
        logger.error("Could not register comet because it had no name.")
      }

    case EventActorClientCometDeregistered(cometName) =>
      registeredComets -= cometName

    case EventSummaryRequested(comet, userId, eventType) =>
      val eventSummaryEntries = {
        for {
          daysBefore <- (0 to 6).toList
        } yield {
          val rangeStart = (new DateTime()).withTimeAtStartOfDay().minusDays(daysBefore)
          val rangeEnd = (new DateTime()).withTimeAtStartOfDay().minusDays(daysBefore - 1)

          val eventTypeCount = Event.count(
            ("userId" -> userId) ~
            ("eventType" -> eventType) ~
            ("createdAt" ->
              ("$gte" -> rangeStart.toString()) ~
              ("$lt" -> rangeEnd.toString())
            )
          )
          EventSummaryEntry(rangeStart.toString("MMM dd"), eventTypeCount)
        }
      }.reverse

      comet ! EventSummaryDelivered(EventSummary(eventType, eventSummaryEntries))
  }
}
