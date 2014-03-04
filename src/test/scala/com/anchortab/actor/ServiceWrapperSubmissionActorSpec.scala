package com.anchortab.actor

import net.liftweb.common._
import net.liftweb.util._
  import Helpers.{intToTimeSpanBuilder => _, _}

import org.scalatest._
  import concurrent.Eventually._
import org.scalatest.time.SpanSugar._

import com.anchortab.model._

import org.joda.time._
import org.bson.types.ObjectId

object ServiceWrapperSubmissionActorSpecExamples {
  lazy val user = {
    val user = User(randomString(32).toLowerCase, randomString(32))
    user.save
    user
  }

  def tabWithService(service: ServiceWrapper) = {
    val tab = Tab(
      randomString(32),
      user._id,
      TabAppearance.defaults,
      service = service
    )

    tab.save
    tab
  }

  lazy val tabWithSuccessfulService = tabWithService(AlwaysSuccessfulServiceWrapper())
  lazy val tabWithFailureService = tabWithService(AlwaysFailureServiceWrapper())
  lazy val tabWithEmptyService = tabWithService(AlwaysEmptyServiceWrapper())
}

class ServiceWrapperSubmissionActorSpec extends FunSpec with ShouldMatchers with BeforeAndAfterAll {
  import ServiceWrapperSubmissionActorSpecExamples._

  override def beforeAll() {
    bootstrap.liftweb.SetupDb.setup

    user
    tabWithSuccessfulService
    tabWithFailureService
    tabWithEmptyService
  }

  override def afterAll() {
    user.delete
    tabWithSuccessfulService.delete
    tabWithFailureService.delete
    tabWithEmptyService.delete
  }

  describe("ServiceWrapperSubmissionActor") {
    it("should increment quota counts for a user after a successful submission") {
      ServiceWrapperSubmissionActor ! SubscribeEmailToServiceWrapper(tabWithSuccessfulService, "bacon1@bacon.com", None)

      eventually(timeout(1 seconds)) {
        val updatedUserCountMatches = User.find(user._id).flatMap(_.quotaCounts.get("email-subscriptions")).map(_ == 1).getOrElse(false)
        updatedUserCountMatches should equal (true)
      }
    }

    it("should increment overall submission count for a tab after successful submission") {
      ServiceWrapperSubmissionActor ! SubscribeEmailToServiceWrapper(tabWithSuccessfulService, "bacon2@bacon.com", None)

      eventually(timeout(1 seconds)) {
        val tabSubscriptionCountMatches = Tab.find(tabWithSuccessfulService._id).map(_.stats.submissions == 2).getOrElse(false)
        tabSubscriptionCountMatches should equal (true)
      }
    }

    it("should correctly handle the case a service generates a failure") {
      ServiceWrapperSubmissionActor ! SubscribeEmailToServiceWrapper(tabWithFailureService, "bacon3@bacon.com", None)

      eventually(timeout(1 seconds)) {
        val tabError =
          Tab.find(tabWithFailureService._id).map(_.errors).getOrElse(Nil).filter(_.email == "bacon3@bacon.com")

        tabError should have length (1)
        tabError(0).service should be ("Always failure")
        tabError(0).message should be ("Chablewit.")
      }
    }

    it("should correctly handle the case a service generates an Empty") {
      ServiceWrapperSubmissionActor ! SubscribeEmailToServiceWrapper(tabWithEmptyService, "bacon3@bacon.com", None)

      eventually(timeout(1 seconds)) {
        val tabError =
          Tab.find(tabWithEmptyService._id).map(_.errors).getOrElse(Nil).filter(_.email == "bacon3@bacon.com")

        tabError should have length (1)
        tabError(0).service should be ("Always empty")
        tabError(0).message should be ("Something unexpected occured and we didn't get an error.")
      }
    }
  }
}
