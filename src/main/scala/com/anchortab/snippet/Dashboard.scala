package com.anchortab.snippet

import net.liftweb._
  import common._
  import mongodb._
  import util.Helpers._
  import json._
    import ext._
    import JsonDSL._
    import Extraction._

object Dashboard {
  def accountStats = {
    val currentPlanName =
      for {
        session <- userSession.is
        user <- session.user
      } yield {
        user.plan.name
      }

    val currentQuotaStats : Map[String, (Long, Long)] =
      {
        import Option.option2Iterable

        for {
          session <- userSession.is
          user <- session.user
        } yield {
          val quotas =
            for {
              quotaName <- user.plan.quotas.keys
              quotaLimit <- user.plan.quotaFor(quotaName)
              quotaUsage <- user.quotaCounts.get(quotaName)
            } yield {
              (quotaName, (quotaLimit, quotaUsage))
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

      ".quota-name *" #> quotaName &
      ".usage *" #> quotaUsage &
      ".maximum *" #> quotaLimit
    }
  }
}
