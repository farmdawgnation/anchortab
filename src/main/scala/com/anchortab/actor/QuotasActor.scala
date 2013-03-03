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
  def messageHandler = {
    case ScheduleQuotaReset =>
      // Schedule next months quota reset.
      val beginningOfNextMonth = (new DateTime()).toDateMidnight.withDayOfMonth(1).plusMonths(1).toDateTime
      val timeSpan = new TimeSpan(Left(beginningOfNextMonth))

      logger.info("Scheduling next quota reset for " + timeSpan)

      Schedule(() => this ! QuotaReset, timeSpan)

    case QuotaReset =>
      logger.info("Runing quota reset.")
      val beginningOfTheMonth = (new DateTime()).toDateMidnight.withDayOfMonth(1).toDate
      val today = new Date()

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
