package com.anchortab.actor

import net.liftweb._
  import common._
  import actor._
  import json.Extraction._
  import mongodb._
    import BsonDSL._
  import util._
    import Helpers._

import org.joda.time._

import com.anchortab.model._

sealed trait RetentionDispatcherActorMessage
case object ScheduleRetentionDispatch extends RetentionDispatcherActorMessage
case object DispatchRetentionEmails extends RetentionDispatcherActorMessage

object RetentionDispatcherActor extends LiftActor with Loggable {
  private def timeSpanUntilNextRetentionDispatch = {
    val beginningOfNextDay = Props.mode match {
      case Props.RunModes.Development => (new DateTime()).plusMinutes(1).getMillis
      case _ => (new DateTime()).plusDays(1).withTimeAtStartOfDay().getMillis
    }
    val now = new DateTime().getMillis

    new TimeSpan(beginningOfNextDay - now)
  }

  private def midnightMinusThree = (new DateTime()).withTimeAtStartOfDay().minusDays(3)
  private def midnightMinusTwo = (new DateTime()).withTimeAtStartOfDay().minusDays(2)

  protected def dispatchRetentionEmailTo(targets: List[User]) = {
    targets.foreach { target =>
      EmailActor ! SendRetentionEmail(target.email)
    }
  }

  def retentionEmailTargets = {
    implicit val formats = Event.formats

    val retentionEmailCandidates = User.findAll(
      ("createdAt" -> (
        ("$gte" -> decompose(midnightMinusThree)) ~
        ("$lt" -> decompose(midnightMinusTwo))
      ))
    )

    retentionEmailCandidates.collect {
      case candidate if Tab.findAll("userId" -> candidate._id).foldLeft(true)(_ && _.stats.views == 0) =>
        candidate
    }
  }

  override def messageHandler = {
    case ScheduleRetentionDispatch =>
      if (Props.productionMode)
        Schedule(() => this ! DispatchRetentionEmails, timeSpanUntilNextRetentionDispatch)

    case DispatchRetentionEmails =>
      dispatchRetentionEmailTo(retentionEmailTargets)

      if (Props.productionMode)
        this ! ScheduleRetentionDispatch
  }
}
