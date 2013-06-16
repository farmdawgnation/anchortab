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
  def totalActiveReferralsAsOfTargetMonth(affiliateId: ObjectId, targetMonth: YearMonth) = {
    AffiliateCalculationResult(affiliateId, targetMonth, 0, Map.empty, AffiliateSummaryType.TotalActiveReferralsAsOfTargetMonth)
  }

  def newReferralsForTargetMonth(affiliateId: ObjectId, targetMonth: YearMonth) = {
    AffiliateCalculationResult(affiliateId, targetMonth, 0, Map.empty, AffiliateSummaryType.NewReferralsForTargetMonth)
  }
}
