package com.anchortab.snippet

import scala.xml.NodeSeq
import scala.math._

import net.liftweb._
  import common._
  import sitemap._
  import mongodb._
  import util._
    import Helpers._
  import json._
    import ext._
    import JsonDSL._
    import Extraction._

import com.anchortab.model._

object Dashboard {
  val dashboardMenu = Menu.i("Dashboard") / "manager" / "dashboard"

  val menus =
    dashboardMenu ::
    Nil

  def weeksPerformanceGraph = {
    {
      for {
        session <- userSession.is
        cometName = "event-summary-comet-" + session.userId
      } yield {
        <lift:comet type="EventSummaryComet" name={cometName}></lift:comet>
      }
    } openOr {
      NodeSeq.Empty
    }
  }

  def accountStats = {
    val currentPlanName =
      for {
        user <- currentUser.is
      } yield {
        user.plan.name
      }

    val currentQuotaStats : Map[String, (Long, Long)] =
      {
        import Option.option2Iterable

        for {
          user <- currentUser.is
        } yield {
          val quotas =
            for {
              quotaName <- user.plan.quotas.keys
              quotaHumanName = Plan.Quotas.humanNameFor(quotaName)
              quotaLimit <- user.plan.quotaFor(quotaName)
              quotaUsage <- user.quotaCounts.get(quotaName) orElse Some(0l)
            } yield {
              (quotaHumanName, (quotaUsage, quotaLimit))
            }

          quotas.toMap
        }
      } openOr {
        Map.empty
      }

    ".plan *" #> currentPlanName &
    ".quota-summary" #> currentQuotaStats.map { quotaStat =>
      val quotaName = quotaStat._1
      val quotaData = quotaStat._2

      val quotaUsage = quotaData._1
      val quotaLimit = quotaData._2
      val usagePercentage = min(((quotaUsage.toDouble / quotaLimit) * 100).toInt, 100)
      val meterClass = {
        if (usagePercentage < 75) {
          "blue"
        } else if (usagePercentage < 100) {
          "orange"
        } else {
          "red"
        }
      }

      ".quota-name *" #> quotaName &
      ".usage *" #> quotaUsage &
      ".maximum *" #> quotaLimit &
      ".usage-meter [style]" #> ("width: " + usagePercentage + "%;") andThen
      ".meter [class+]" #> meterClass
    }
  }
}
