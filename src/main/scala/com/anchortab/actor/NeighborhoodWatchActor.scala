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

sealed trait NeighborhoodWatchMessage
case object ScheduleNeighborhoodWatch extends NeighborhoodWatchMessage
case object NeighborhoodWatch extends NeighborhoodWatchMessage

sealed trait NeighborhoodWatchResult
case class SameSiteMultipleAccount(domain: String, accounts: List[String]) extends NeighborhoodWatchResult
case class MultipleAccountsSameIp(accounts: List[String], ip: String) extends NeighborhoodWatchResult
case class SimilarEmailAddresses(accounts: List[String]) extends NeighborhoodWatchResult

object NeighborhoodWatchActor extends LiftActor with Loggable {
  private def timeSpanUntilNextNeighborhoodWatch = {
    val beginningOfNextMonth = Props.mode match {
      case Props.RunModes.Development => (new DateTime()).plusMinutes(1).getMillis
      case _ => (new DateTime()).toDateMidnight.plusWeeks(1).withDayOfWeek(1).getMillis
    }
    val now = new DateTime().getMillis

    new TimeSpan(beginningOfNextMonth - now)
  }

  private def accountsWithSameIp: List[MultipleAccountsSameIp] = {
    implicit val formats = UserSession.formats

    val recentSessions = UserSession.findAll(
      ("createdAt" -> ("$gt" -> decompose(DateMidnight.now().minusWeeks(1))))
    )

    // A list of IPs mapped to a tuple3 of userID, ip, and agent.
    val heuristicData = recentSessions.map({session =>
      (session.userId.toString, session.ip)
    }).distinct.groupBy(_._2)

    heuristicData.filter(_._2.size > 1).collect({
      case (ipAddress, identitySummaries) if identitySummaries.map(_._1).distinct.size > 1 =>
        MultipleAccountsSameIp(
          identitySummaries.map(_._1).distinct,
          ipAddress
        )
    }).toList
  }

  private def accountsWithSimilarEmails: List[SimilarEmailAddresses] = {
    implicit val formats = User.formats

    val usersWithAPlusAddress = User.findAll("email" -> "\\w+\\+\\w+@\\w+\\.\\w+".r)

    val usersWithMatchingFirstAndLastPiece = usersWithAPlusAddress.groupBy({ user =>
      "(\\w+)\\+\\w+@(\\w+\\.\\w+)".r.findAllIn(user.email).matchData.map(_.subgroups).toList.flatten
    })

    usersWithMatchingFirstAndLastPiece.collect({
      case (_, users) if users.size > 1 =>
        SimilarEmailAddresses(users.map(_.email))
    }).toList
  }

  private def domainsUsingMultipleAccounts: List[SameSiteMultipleAccount] = {
    implicit val formats = Event.formats

    val recentEventsWithDomain = Event.findAll(
      ("createdAt" -> ("$gt" -> decompose(DateMidnight.now().minusMonths(1)))) ~
      ("domain" -> ("$exists" -> true))
    )

    recentEventsWithDomain.groupBy(_.domain).mapValues(_.map(_.userId.toString)).collect({
      case (Some(domain), userIds) if userIds.size > 1 =>
        SameSiteMultipleAccount(domain, userIds)
    }).toList
  }

  override def messageHandler = {
    case ScheduleNeighborhoodWatch =>
      val delay = timeSpanUntilNextNeighborhoodWatch

      logger.info("Scheduling next neighborhood watch for " + delay)

      Schedule(() => this ! NeighborhoodWatch, delay)

    case NeighborhoodWatch =>
      logger.info("Running neighborhood watch.")

      logger.info(accountsWithSimilarEmails)

      this ! ScheduleNeighborhoodWatch
  }
}
