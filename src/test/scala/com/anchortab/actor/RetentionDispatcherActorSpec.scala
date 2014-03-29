package com.anchortab.actor

import net.liftweb.common._
import net.liftweb.util._
  import Helpers._

import org.scalatest._

import com.anchortab.model._

import org.joda.time._

import org.bson.types.ObjectId

object RetentionDispatcherActorSpecExamples {
  val midnightMinusThree = (new DateTime()).withTimeAtStartOfDay().minusDays(3)
  val midnightMinusTwo = (new DateTime()).withTimeAtStartOfDay().minusDays(2)

  def userWithCreatedAt(createdAt: DateTime) = {
    val user = User(randomString(32).toLowerCase, randomString(32), createdAt = createdAt)
    user.save
    user
  }

  def giveUserTab(user: User, views: Int) = {
    val tab = Tab(randomString(32), user._id, TabAppearance.defaults, AlwaysSuccessfulServiceWrapper(), stats = TabStats(views))
    tab.save
    tab
  }

  lazy val userDMinus4 = userWithCreatedAt(midnightMinusThree.minusDays(1))
  lazy val userDMinus1 = userWithCreatedAt(midnightMinusTwo.plusDays(1))

  lazy val userDMinus3a = userWithCreatedAt(midnightMinusThree)
  lazy val userDMinus2a = userWithCreatedAt(midnightMinusTwo.minusHours(2))
  lazy val tab2a = giveUserTab(userDMinus2a, 0)

  lazy val userDMinus3b = userWithCreatedAt(midnightMinusThree)
  lazy val userDMinus2b = userWithCreatedAt(midnightMinusTwo.minusHours(2))

  lazy val tab3b = giveUserTab(userDMinus3b, 1)
  lazy val tab2b = giveUserTab(userDMinus2b, 5)
}

class RetentionDispatcherActorSpec extends FunSpec with ShouldMatchers with BeforeAndAfterAll {
  import RetentionDispatcherActorSpecExamples._

  override def beforeAll() {
    bootstrap.liftweb.SetupDb.setup

    userDMinus4
    userDMinus1
    userDMinus3a
    userDMinus2a
    tab2a
    userDMinus3b
    userDMinus2b
    tab3b
    tab2b
  }

  override def afterAll() {
    userDMinus4.delete
    userDMinus1.delete
    userDMinus3a.delete
    userDMinus2a.delete
    tab2a.delete
    userDMinus3b.delete
    userDMinus2b.delete
    tab3b.delete
    tab2b.delete
  }

  describe("RetentionDispatcherActor") {
    lazy val targets = RetentionDispatcherActor.retentionEmailTargets

    it("targets a user createdAt D-3 and D-2 w/ no viewed tabs") {
      targets should contain(userDMinus3a)
      targets should contain(userDMinus2a)
    }

    it("ignores a user createdAt D-3 and D-2 w/ views on tabs") {
      targets should not contain(userDMinus3b)
      targets should not contain(userDMinus2b)
    }

    it("ignores a user createdAt D-4 w/ no viewed tabs") {
      targets should not contain(userDMinus4)
    }

    it("ignores a user createdAt D-1 w/ no viewed tabs") {
      targets should not contain(userDMinus1)
    }
  }
}
