package com.anchortab.actor

import scala.xml.NodeSeq

import net.liftweb._
  import common._
  import actor._
  import mongodb._
    import BsonDSL._
  import util.Helpers._
  import json._
    import ext._
    import Extraction._
  import util._
  import http._

import com.anchortab.model._

import org.joda.time._

import java.util.Date

import org.bson.types.ObjectId

case object ScheduleQuotaReset
case object QuotaReset
case class CheckQuotaCounts(userId: ObjectId)

object QuotasActor extends LiftActor with Loggable {
  private def timeSpanUntilNextQuotaReset = {
    val beginningOfNextMonth = Props.mode match {
      case Props.RunModes.Development => (new DateTime()).plusMinutes(5).getMillis
      case _ => (new DateTime()).toDateMidnight.withDayOfMonth(1).plusMonths(1).getMillis
    }
    val now = new DateTime().getMillis

    new TimeSpan(beginningOfNextMonth - now)
  }

  def messageHandler = {
    case ScheduleQuotaReset =>
      // Schedule next months quota reset.
      val delay = timeSpanUntilNextQuotaReset

      logger.info("Scheduling next quota reset for " + delay)

      Schedule(() => this ! QuotaReset, delay)

    case QuotaReset =>
      logger.info("Runing quota reset.")

      // Todo: Fix me to filter by those not updated since the last month.
      User.update(
        JObject(Nil),
        (
          "$unset" -> (
            (("quotaCounts." + Plan.Quotas.Views) -> true) ~
            (("quotaCounts." + Plan.Quotas.EmailSubscriptions) -> true)
          )
        ),
        Multi
      )

      // Schedule next month's reset
      this ! ScheduleQuotaReset

    case CheckQuotaCounts(userId) =>
      for {
        user <- User.find(userId)
          if ! user.admin_?
      } {
        logger.info("Checking quota for " + user.email)
        if (! user.withinQuotaFor_?(Plan.Quotas.Views) ||
            ! user.withinQuotaFor_?(Plan.Quotas.EmailSubscriptions)) {
          EmailActor ! SendQuotaErrorEmail(user.email)
        } else if(user.nearQuotaFor_?(Plan.Quotas.Views) ||
                  user.nearQuotaFor_?(Plan.Quotas.EmailSubscriptions)) {
          EmailActor ! SendQuotaWarningEmail(user.email)
        }
      }
  }
}
