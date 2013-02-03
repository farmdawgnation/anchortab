package com.anchortab.comet

import scala.xml.NodeSeq

import net.liftweb._
  import common._
  import http._
  import js._
    import JsCmds._

import com.anchortab.actor._
import com.anchortab.snippet._
import com.anchortab.model._

import org.bson.types.ObjectId

case class EventSummaryDelivered(eventSummary: EventSummary)

case class EventSummaryUpdated(eventSummary: EventSummary)
  extends SimpleAnchorTabEvent("event-summary-updated")

class EventSummaryComet extends CometActor {
  case object RequestEventSummary extends CallableFunction("requestEventSummary", requestEventSummary)

  override def localSetup = {
    EventActor ! EventActorClientCometRegistered(this)
  }

  override def localShutdown = {
    for {
      name <- this.name
    } {
      EventActor ! EventActorClientCometDeregistered(name)
    }
  }

  def render = {
    RequestEventSummary
  }

  def requestEventSummary(s: String) = {
    for {
      name <- this.name
      userIdStr = name.replace("event-summary-comet-", "")
      userId = new ObjectId(userIdStr)
    } {
      EventActor ! EventSummaryRequested(this, userId, Event.Types.TabView)
      EventActor ! EventSummaryRequested(this, userId, Event.Types.TabSubmit)
    }

    Noop
  }

  override def lowPriority: PartialFunction[Any, Unit] ={
    case EventSummaryDelivered(eventSummary) =>
      partialUpdate(EventSummaryUpdated(eventSummary))
  }
}
