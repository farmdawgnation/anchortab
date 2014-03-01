package com.anchortab.actor

import net.liftweb.common._
import net.liftweb.util._
  import Helpers.{intToTimeSpanBuilder => _, _}
import net.liftweb.actor._

import org.scalatest._
  import concurrent.Eventually._
import org.scalatest.time.SpanSugar._

import com.anchortab.model._

import org.joda.time._
import org.bson.types.ObjectId

object ErrorNotifierActorSpecExamples {
  val yesterday = (new DateTime().withTimeAtStartOfDay().minusDays(1))

  lazy val user = {
    val user = User(randomString(32).toLowerCase, randomString(32))
    user.save
    user
  }

  def tabWithErrorList(errorList: List[TabError]) = {
    val tab = Tab(
      randomString(32),
      user._id,
      TabAppearance.defaults,
      errors = errorList
    )

    tab.save
    tab
  }

  val errorFromTwoDaysAgo = TabError("zz", "zz", "zz", None, yesterday.minusDays(1).plusHours(2))
  val errorFromYesterday = TabError("zz", "zz", "zz", None, yesterday.plusHours(2))
  val errorFromToday = TabError("zz", "zz", "zz", None, yesterday.plusHours(2).plusDays(1))

  lazy val tab1 = tabWithErrorList(errorFromTwoDaysAgo :: Nil)
  lazy val tab2 = tabWithErrorList(errorFromTwoDaysAgo :: errorFromYesterday :: Nil)
  lazy val tab3 = tabWithErrorList(errorFromToday :: Nil)
}
class ErrorNotifierActorSpec extends FunSpec with ShouldMatchers with BeforeAndAfterAll {
  import ErrorNotifierActorSpecExamples._

  override def beforeAll() {
    bootstrap.liftweb.SetupDb.setup

    user
    tab1
    tab2
    tab3
  }

  override def afterAll() {
    user.delete
    tab1.delete
    tab2.delete
    tab3.delete
  }

  describe("ErrorNotifierActor") {
    it("should trigger SendSubmitErrorNotificationEmail for tabs with recent errors") {
      val mockEmailActor = new MockLiftActor {}
      val mockErrorNotifierActor = new ErrorNotifierActor {
        val emailActor = mockEmailActor
      }

      mockErrorNotifierActor ! CheckForEmailSubmissionErrors

      eventually(timeout(1 seconds)) {
        val firstMessage = mockEmailActor.messages(0)

        firstMessage match {
          case thing @ SendSubmitErrorNotificationEmail(userEmail, tabs) =>
            println(thing)
            userEmail should be (user.email)

            tabs should contain (tab2)
            tabs should contain (tab3)

          case _ =>
            fail("Message didn't match.")
        }
      }
    }
  }
}
