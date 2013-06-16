package com.anchortab.snippet

import net.liftweb.common._
import net.liftweb.util._
  import Helpers._

import org.scalatest._

import com.anchortab.model._

import org.joda.time._

import org.bson.types.ObjectId

object AffiliateCalculationSpecExamples {
  val targetMonth: YearMonth = new YearMonth(2013, 05)

  def affiliateUserWithCode = {
    val user = User(
      email = randomString(32),
      password = randomString(32),
      affiliateCode = Some(randomString(32))
    )

    user.save

    user
  }

  def referredUser(referringUserId: ObjectId, begins: DateTime, ends: Option[DateTime] = None) = {
    val userSubs = List(
      UserSubscription(
        testReferralPlan._id, testReferralPlan.price, testReferralPlan.term,
        begins = begins,
        ends = ends
      )
    )

    val user = User(
      email = randomString(32),
      password = randomString(32),
      referringAffiliateId = Some(referringUserId),
      subscriptions = userSubs
    )

    user.save

    user
  }

  def referredUserWithTwoSubs(referringUserId: ObjectId, currentSubBegins: DateTime, currentSubEnds: Option[DateTime] = None) = {
    val userSubs = List(
      UserSubscription(
        testReferralPlan._id, testReferralPlan.price, testReferralPlan.term,
        begins = currentSubBegins.minusMonths(2),
        ends = Some(currentSubBegins.minusMonths(2).plusDays(15))
      ),
      UserSubscription(
        testReferralPlan._id, testReferralPlan.price, testReferralPlan.term,
        begins = currentSubBegins,
        ends = currentSubEnds
      )
    )

    val user = User(
      email = randomString(32),
      password = randomString(32),
      referringAffiliateId = Some(referringUserId),
      subscriptions = userSubs
    )

    user.save

    user
  }

  val dateBeforeTargetMonth = targetMonth.toDateTime(new DateTime()).minusMonths(1)
  val dateAfterTargetMonth = targetMonth.toDateTime(new DateTime()).plusMonths(1)
  val dateInTargetMonth = targetMonth.toDateTime(new DateTime()).withDayOfMonth(2)
  val dateTwentyOneDaysAfterDateInTargetMonth = dateInTargetMonth.plusDays(21)
  val dateTwoDaysAfterDateInTargetMonth = dateInTargetMonth.plusDays(2)

  lazy val testReferralPlan = {
    val plan = Plan("Test Plan", "A test plan.", 10.0, 0, Map.empty, Map.empty)

    plan.save

    plan
  }

  // A user whose subscription started before the target month with no ending specified.
  lazy val affiliateUserA = affiliateUserWithCode
  lazy val referredUserA = referredUser(affiliateUserA._id, dateBeforeTargetMonth)

  // A user whose subscription started before the target month with an ending after
  // the target month.
  lazy val affiliateUserB = affiliateUserWithCode
  lazy val referredUserB = referredUser(affiliateUserB._id, dateBeforeTargetMonth, Some(dateAfterTargetMonth))

  // A user who started in the target month with no end date specified.
  lazy val affiliateUserC = affiliateUserWithCode
  lazy val referredUserC = referredUser(affiliateUserC._id, dateInTargetMonth)

  // A user who started in the target month and their end date is twenty days after the start
  // date
  lazy val affiliateUserD = affiliateUserWithCode
  lazy val referredUserD = referredUser(affiliateUserD._id, dateInTargetMonth, Some(dateTwentyOneDaysAfterDateInTargetMonth))

  // A user who started in the target month, and almost immeidately canceled.
  lazy val affiliateUserE = affiliateUserWithCode
  lazy val referredUserE = referredUser(affiliateUserE._id, dateInTargetMonth, Some(dateTwoDaysAfterDateInTargetMonth))

  // A user with two subscriptions whose start date started in the target month.
  lazy val affiliateUserF = affiliateUserWithCode
  lazy val referredUserF = referredUserWithTwoSubs(affiliateUserF._id, dateInTargetMonth)
}

class AffiliateCalculationSpec extends FunSpec with ShouldMatchers with BeforeAndAfterAll {
  import AffiliateCalculationSpecExamples._

  object TestAffiliateCalculator extends AffiliateCalculation

  def totalPlusOneTestForAffiliateUser(user: User) = {
    val calculation = TestAffiliateCalculator.totalActiveReferralsAsOfTargetMonth(user._id, targetMonth)

    calculation.total should equal (1)
    calculation.totalByPlan.get(testReferralPlan.name) should equal (Some(1))
  }

  def totalPlusZeroTestForAffiliateUser(user: User) = {
    val calculation = TestAffiliateCalculator.totalActiveReferralsAsOfTargetMonth(user._id, targetMonth)

    calculation.total should equal (0)
    calculation.totalByPlan.get(testReferralPlan.name) should equal (Some(0))
  }

  def monthPlusOneTestForAffiliateUser(user: User) = {
    val calculation = TestAffiliateCalculator.newReferralsForTargetMonth(user._id, targetMonth)

    calculation.total should equal (1)
    calculation.totalByPlan.get(testReferralPlan.name) should equal (Some(1))
  }

  def monthPlusZeroTestForAffiliateUser(user: User) = {
    val calculation = TestAffiliateCalculator.newReferralsForTargetMonth(user._id, targetMonth)

    calculation.total should equal (0)
    calculation.totalByPlan.get(testReferralPlan.name) should equal (Some(0))
  }

  override def beforeAll() {
    bootstrap.liftweb.SetupDb.setup

    affiliateUserA
    referredUserA

    affiliateUserB
    referredUserB

    affiliateUserC
    referredUserC

    affiliateUserD
    referredUserD

    affiliateUserE
    referredUserE

    affiliateUserF
    referredUserF
  }

  override def afterAll() {
    affiliateUserA.delete
    referredUserA.delete

    affiliateUserB.delete
    referredUserB.delete

    affiliateUserC.delete
    referredUserC.delete

    affiliateUserD.delete
    referredUserD.delete

    affiliateUserE.delete
    referredUserE.delete

    affiliateUserF.delete
    referredUserF.delete
  }

  describe("Total as of Target Month Calculator") {
    it("+1 for each paid sub w/ code; start before target month; no end date") {
      totalPlusOneTestForAffiliateUser(affiliateUserA)
    }

    it("+1 for each paid sub w/ code; start before target month; end date after target mo") {
      totalPlusOneTestForAffiliateUser(affiliateUserB)
    }

    it("+1 for each paid sub w/ code; start in target mo; without end date") {
      totalPlusOneTestForAffiliateUser(affiliateUserC)
    }

    it("+1 for each paid sub w/ code; start in target mo; end date 20 days after start") {
      totalPlusOneTestForAffiliateUser(affiliateUserD)
    }

    it("+0 for paid sub; start in target mo; cancel two days later") {
      totalPlusZeroTestForAffiliateUser(affiliateUserE)
    }
  }

  describe("New Users for Target Month Calculator") {
    it("+1 for first sub by new user w/ code; start in target mo; no end date") {
      monthPlusOneTestForAffiliateUser(affiliateUserC)
    }

    it("+1 for first sub by new user w/ code; start in target mo; end date > 20 days after start") {
      monthPlusOneTestForAffiliateUser(affiliateUserD)
    }

    it("+0 for first sub by new user w/ code; start in target mo; end two day later") {
      monthPlusZeroTestForAffiliateUser(affiliateUserE)
    }

    it("+0 for second sub in target month; no end date") {
      monthPlusZeroTestForAffiliateUser(affiliateUserF)
    }
  }
}
