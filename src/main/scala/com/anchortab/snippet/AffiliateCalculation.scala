package com.anchortab.snippet

import net.liftweb._
  import common._
  import util._
  import mongodb._
    import BsonDSL._

import com.anchortab.model._

import org.bson.types.ObjectId

import org.joda.time._

object AffiliateSummaryType {
  val TotalActiveReferralsAsOfTargetMonth = "total-active-referrals-as-of-target-month"
  val NewReferralsForTargetMonth = "new-referrals-for-target-month"
}

case class AffiliateCalculationResult(affiliateId: ObjectId, targetMonth: YearMonth, total: Int,
    totalByPlan: Map[String, Int], summaryType: String)

trait AffiliateCalculation extends Loggable {
  private def subscriptionsDuringTargetMonth(targetMonth: YearMonth, user: User) = {
    val startOfTargetMonth = targetMonth.toDateTime(new DateMidnight()).withDayOfMonth(1)
    val startOfNextMonth = startOfTargetMonth.plusMonths(1)

    user.subscriptions.sortWith(_.begins isBefore _.begins).filter { subscription =>
      (
        (
          subscription.begins.getMonthOfYear() == targetMonth.getMonthOfYear() &&
          subscription.begins.getYear() == targetMonth.getYear()
        ) ||
        (subscription.begins isBefore startOfTargetMonth)
      ) &&
      (
        (! subscription.ends.isDefined) ||
        (subscription.ends.get isAfter startOfNextMonth) ||
        (subscription.ends.get isAfter subscription.begins.plusDays(20))
      )
    }
  }

  def totalActiveReferralsAsOfTargetMonth(affiliateId: ObjectId, targetMonth: YearMonth) = {
    val usersReferred = User.findAll("referringAffiliateId" -> affiliateId)

    val usersSubscribedDuringTargetMonth = usersReferred.collect {
      case user if subscriptionsDuringTargetMonth(targetMonth, user).nonEmpty =>
        val mostRecentSub = subscriptionsDuringTargetMonth(targetMonth, user).last

        (user, mostRecentSub.plan.map(_.name).getOrElse(""))
    }

    val userSubscriptionsByPlan = usersSubscribedDuringTargetMonth.groupBy(_._2).map({
      case (planName, users) => (planName, users.size)
    }).toMap

    println(userSubscriptionsByPlan)

    AffiliateCalculationResult(affiliateId, targetMonth, usersSubscribedDuringTargetMonth.size, userSubscriptionsByPlan, AffiliateSummaryType.TotalActiveReferralsAsOfTargetMonth)
  }

  def newReferralsForTargetMonth(affiliateId: ObjectId, targetMonth: YearMonth) = {
    AffiliateCalculationResult(affiliateId, targetMonth, 0, Map.empty, AffiliateSummaryType.NewReferralsForTargetMonth)
  }
}
