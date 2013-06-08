package com.anchortab.actor

import net.liftweb._
  import common._
  import actor._
  import mongodb._
    import BsonDSL._
  import util._
    import Helpers._

import org.joda.time._

sealed trait NeighborhoodWatchMessage
case object ScheduleNeighborhoodWatch extends NeighborhoodWatchMessage
case object NeighborhoodWatch extends NeighborhoodWatchMessage

sealed trait NeighborhoodWatchResult
case class SameSiteMultipleAccount(domain: String, accounts: List[String]) extends NeighborhoodWatchResult
case class MultipleAccountsSameIpAndUserAgent(accounts: List[String], ip: String, userAgent: String) extends NeighborhoodWatchResult
case class SimilarEmailAddresses(accounts: List[String]) extends NeighborhoodWatchResult

object NeighborhoodWatchActor extends LiftActor with Loggable {
  private def timeSpanUntilNextNeighborhoodWatch = {
    val beginningOfNextMonth = Props.mode match {
      //case Props.RunModes.Development => (new DateTime()).plusMinutes(5).getMillis
      case _ => (new DateTime()).toDateMidnight.plusWeeks(1).withDayOfWeek(1).getMillis
    }
    val now = new DateTime().getMillis

    new TimeSpan(beginningOfNextMonth - now)
  }

  override def messageHandler = {
    case ScheduleNeighborhoodWatch =>
      val delay = timeSpanUntilNextNeighborhoodWatch

      logger.info("Scheduling next neighborhood watch for " + delay)

      Schedule(() => this ! NeighborhoodWatch, delay)

    case NeighborhoodWatch =>
      logger.info("Running neighborhood watch.")
  }
}
