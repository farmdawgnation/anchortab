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

sealed trait ErrorNotifierMessage
case object ScheduleErrorNotifierChecks extends ErrorNotifierMessage
case object CheckForEmailSubmissionErrors extends ErrorNotifierMessage

object ErrorNotifierActor extends ErrorNotifierActor {
  val emailActor = EmailActor
}
trait ErrorNotifierActor extends LiftActor with Loggable {
  def emailActor: LiftActor

  private def timeSpanUntilNextEmailSubmissionCheck = {
    val beginningOfNextDay = Props.mode match {
      case Props.RunModes.Development => (new DateTime()).plusMinutes(1).getMillis
      case _ => (new DateTime()).plusDays(1).withTimeAtStartOfDay().getMillis
    }
    val now = new DateTime().getMillis

    new TimeSpan(beginningOfNextDay - now)
  }

  private def yesterday = (new DateTime().withTimeAtStartOfDay().minusDays(1))

  private def checkForEmailSubmissionErrors = {
    implicit val formats = Event.formats

    val erroredTabs = Tab.findAll(
      ("errors.createdAt" -> ("$gt" -> decompose(yesterday)))
    )

    if (erroredTabs.isEmpty)
      logger.info("Found no errored tabs for time period.")

    erroredTabs.groupBy(_.userId).foreach {
      case (userId, erroredTabsForUser) =>
        for (user <- User.find(userId)) {
          emailActor ! SendSubmitErrorNotificationEmail(user.email, erroredTabsForUser)
        }

      case _ =>
    }
  }

  override def messageHandler = {
    case ScheduleErrorNotifierChecks =>
      if (Props.productionMode)
        Schedule(() => this ! CheckForEmailSubmissionErrors, timeSpanUntilNextEmailSubmissionCheck)

    case CheckForEmailSubmissionErrors =>
      checkForEmailSubmissionErrors
  }
}
