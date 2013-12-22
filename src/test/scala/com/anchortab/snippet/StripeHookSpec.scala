package com.anchortab.snippet

import org.scalatest._

import net.liftweb.actor._
import net.liftweb.mockweb._
  import MockWeb._
import net.liftweb.mocks._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.json._
  import JsonDSL._
  import Extraction._
import net.liftweb.util._
  import Helpers._

import com.anchortab.actor._
import com.anchortab.model._

import org.bson.types.ObjectId

import org.joda.time._

object StripeHookSpecExamples {
  def customer(activeCard: Boolean = false, activeSubscription: Option[ObjectId] = None) = {
    var user = User(
      email = randomString(32),
      password = randomString(32),
      stripeCustomerId = Some(randomString(32))
    )

    if (activeCard)
      user = user.copy(activeCard = Some(UserActiveCard("1234", "mc", 4, 2013)))

    activeSubscription.foreach { planId =>
      user = user.copy(subscriptions = List(UserSubscription(
        planId,
        10.0,
        Plan.MonthlyTerm,
        status = "active"
      )))
    }

    user.save

    user
  }

  def plan = {
    val plan = Plan(
      randomString(32),
      randomString(32),
      10.0,
      0,
      Map.empty,
      Map.empty,
      stripeId = Some(randomString(32))
    )

    plan.save

    plan
  }

  lazy val plan1 = plan
  lazy val plan2 = plan

  lazy val customer1 = customer()
  lazy val customer2 = customer(activeCard = true)
  lazy val customer3 = customer(activeCard = true)
  lazy val customer4 = customer(activeCard = true)
  lazy val customer5 = customer(activeCard = true, activeSubscription = Some(plan1._id))
  lazy val customer6 = customer(activeCard = true, activeSubscription = Some(plan1._id))
  lazy val customer7 = customer(activeCard = true, activeSubscription = Some(plan1._id))
  lazy val customer8 = customer(activeCard = true, activeSubscription = Some(plan1._id))
  lazy val customer9 = customer(activeCard = true, activeSubscription = Some(plan1._id))
}

class StripeHookSpec extends FunSpec with ShouldMatchers with BeforeAndAfterAll {
  implicit val formats = DefaultFormats
  import StripeHookSpecExamples._

  MockWeb.useLiftRulesGlobally = true

  trait MockEmailActor extends MockLiftActor with EmailActor

  protected def withStripeHookAndEmailActor(testFn: (StripeHook, MockEmailActor)=>Any) {
    val mockEmailActor = new MockEmailActor {}
    val stripeHook = new StripeHook {
      val emailActor = mockEmailActor
    }

    testFn(stripeHook, mockEmailActor)
  }

  def runStripeHookRequest(stripeHook: StripeHook, body: JValue, requestModifier: (MockHttpServletRequest)=>Any = (req)=>false)(responseHandler: (Box[LiftResponse]=>Any)) = {
    val mockReq = new MockHttpServletRequest("http://test.anchortab.com/stripe-hook")
    mockReq.method = "POST"
    mockReq.body = compact(render(body)).getBytes("UTF-8")

    requestModifier(mockReq)

    testS(mockReq) {
      testReq(mockReq) { req =>
        responseHandler(stripeHook(req)())
      }
    }
  }

  override def beforeAll() {
    bootstrap.liftweb.SetupDb.setup

    customer1
    customer2
    customer3
    customer4
    customer5
    customer6
    customer7
    customer8
    customer9

    plan1
    plan2
  }

  override def afterAll() {
    customer1.delete
    customer2.delete
    customer3.delete
    customer4.delete
    customer5.delete
    customer6.delete
    customer7.delete
    customer8.delete
    customer9.delete

    plan1.delete
    plan2.delete
  }

  def okResponseTest(stripeData: JValue)(additionalAssertions: (MockLiftActor)=>Unit) = {
    withStripeHookAndEmailActor { (stripeHook, emailActor) =>
      runStripeHookRequest(stripeHook, stripeData) { response =>
        response match {
          case Full(response) =>
            response should equal (OkResponse())
            additionalAssertions(emailActor)

          case other =>
            fail("got: " + other)
        }
      }
    }
  }

  describe("POST /stripe-hook") {
    describe("general processing") {
      it("returns an empty if attempting to process an event id twice") {
        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "bacon") ~
          ("data" -> ("object" -> JObject(Nil)))

        withStripeHookAndEmailActor { (stripeHook, emailActor) =>
          runStripeHookRequest(stripeHook, stripeData)(_ => false)

          runStripeHookRequest(stripeHook, stripeData) { response =>
            response should equal (Empty)
          }
        }
      }

      it("returns an Empty if the event type is missing") {
        val stripeData =
          ("id" -> randomString(32)) ~
          ("data" -> ("object" -> JObject(Nil)))

        withStripeHookAndEmailActor { (stripeHook, emailActor) =>
          runStripeHookRequest(stripeHook, stripeData)(_ => false)

          runStripeHookRequest(stripeHook, stripeData) { response =>
            response should equal (Empty)
          }
        }
      }

      it("returns an Empty if the data attribute is missing") {
        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "bacon")

        withStripeHookAndEmailActor { (stripeHook, emailActor) =>
          runStripeHookRequest(stripeHook, stripeData)(_ => false)

          runStripeHookRequest(stripeHook, stripeData) { response =>
            response should equal (Empty)
          }
        }
      }

      it("returns an Empty if the data object attribute is missing") {
        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "bacon") ~
          ("data" -> JObject(Nil))

        withStripeHookAndEmailActor { (stripeHook, emailActor) =>
          runStripeHookRequest(stripeHook, stripeData)(_ => false)

          runStripeHookRequest(stripeHook, stripeData) { response =>
            response should equal (Empty)
          }
        }
      }
    }

    describe("invoice.payment_succeeded") {
      it("should alert a user when an invoice payment succeeds") {
        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "invoice.payment_succeeded") ~
          ("data" -> ("object" -> (
            ("customer" -> customer1.stripeCustomerId) ~
            ("total" -> 10000)
          )))

        okResponseTest(stripeData) { (emailActor) =>
          val expectedEmailActorMessage = SendInvoicePaymentSucceededEmail(customer1.email.toLowerCase, 100.0)
          emailActor.messages should contain (expectedEmailActorMessage)
        }
      }
    }

    describe("invoice.payment_failed") {
      it("should properly alert a user when a payment has failed") {
        val tomorrow = (new DateTime()).plusDays(1)
        val tomorrowSeconds = tomorrow.getMillis / 1000
        val tomorrowFromSections = new DateTime(tomorrowSeconds * 1000)

        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "invoice.payment_failed") ~
          ("data" -> ("object" -> (
            ("customer" -> customer2.stripeCustomerId) ~
            ("total" -> 10000) ~
            ("next_payment_attempt" -> tomorrowSeconds)
          )))

        okResponseTest(stripeData) { (emailActor) =>
          val expectedEmailActorMessage = SendInvoicePaymentFailedEmail(
            customer2.email.toLowerCase,
            100.0,
            Some(tomorrowFromSections)
          )
          emailActor.messages should contain (expectedEmailActorMessage)
        }
      }

      it("should properly alert a user when the final attempt has failed") {
        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "invoice.payment_failed") ~
          ("data" -> ("object" -> (
            ("customer" -> customer2.stripeCustomerId) ~
            ("total" -> 10000)
          )))

        okResponseTest(stripeData) { (emailActor) =>
          val expectedEmailActorMessage = SendInvoicePaymentFailedEmail(
            customer2.email.toLowerCase,
            100.0,
            None
          )
          emailActor.messages should contain (expectedEmailActorMessage)
        }
      }
    }

    describe("customer.subscription.created") {
      it("should create a new active sub in our db for a user without a sub") {
        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "customer.subscription.created") ~
          ("data" -> ("object" -> (
            ("customer" -> customer3.stripeCustomerId) ~
            ("plan" -> ("id" -> plan1.stripeId)) ~
            ("status" -> "active")
          )))

        okResponseTest(stripeData) { (emailActor) =>
          val subscription = User.find(customer3._id).get.subscription.get

          subscription.planId should equal (plan1._id)
          subscription.price should equal (plan1.price)
          subscription.term should equal (plan1.term)
          subscription.status should equal ("active")

        }
      }

      it("should create a new trailing sub in our db for a user without a sub") {
        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "customer.subscription.created") ~
          ("data" -> ("object" -> (
            ("customer" -> customer4.stripeCustomerId) ~
            ("plan" -> ("id" -> plan1.stripeId)) ~
            ("status" -> "trialing")
          )))

        okResponseTest(stripeData) { (emailActor) =>
          val subscription = User.find(customer4._id).get.subscription.get

          subscription.planId should equal (plan1._id)
          subscription.price should equal (plan1.price)
          subscription.term should equal (plan1.term)
          subscription.status should equal ("trial")
        }
      }

      it("should end an old subscription when a new one is created") {
        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "customer.subscription.created") ~
          ("data" -> ("object" -> (
            ("customer" -> customer3.stripeCustomerId) ~
            ("plan" -> ("id" -> plan2.stripeId)) ~
            ("status" -> "active")
          )))

        okResponseTest(stripeData) { (emailActor) =>
          val user = User.find(customer3._id).get
          val subscription = user.subscription.get
          val oldSubscription = user.subscriptions.find(_.planId == plan1._id).get

          oldSubscription.status should equal ("cancelled")

          subscription.planId should equal (plan2._id)
          subscription.price should equal (plan2.price)
          subscription.term should equal (plan2.term)
          subscription.status should equal ("active")
        }
      }
    }

    describe("customer.subscription.updated") {
      it("should set up a canceled active subscription to cancel at period end") {
        val tomorrow = (new DateTime()).plusDays(1)
        val tomorrowSeconds = tomorrow.getMillis / 1000
        val tomorrowFromSecs = new DateTime(tomorrowSeconds * 1000)

        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "customer.subscription.updated") ~
          ("data" -> ("object" -> (
            ("customer" -> customer7.stripeCustomerId) ~
            ("plan" -> ("id" -> plan1.stripeId)) ~
            ("status" -> "active") ~
            ("cancel_at_period_end" -> true) ~
            ("current_period_end" -> tomorrowSeconds)
          )))

        okResponseTest(stripeData) { (emailActor) =>
          val user = User.find(customer7._id).get
          val subscription = user.subscription.get

          user.subscriptions should have length 1
          subscription.status should equal ("cancelled")
          subscription.ends.get should equal (tomorrowFromSecs)
        }
      }

      it("should clear ending date and reactivate if cancelled subscription is reactivated") {
        val tomorrow = (new DateTime()).plusDays(1)
        val tomorrowSeconds = tomorrow.getMillis / 1000
        val tomorrowFromSecs = new DateTime(tomorrowSeconds * 1000)

        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "customer.subscription.updated") ~
          ("data" -> ("object" -> (
            ("customer" -> customer7.stripeCustomerId) ~
            ("plan" -> ("id" -> plan1.stripeId)) ~
            ("status" -> "active") ~
            ("cancel_at_period_end" -> false) ~
            ("current_period_end" -> tomorrowSeconds)
          )))

        okResponseTest(stripeData) { (emailActor) =>
          val user = User.find(customer7._id).get
          val subscription = user.subscription.get

          user.subscriptions should have length 1
          subscription.status should equal ("active")
          subscription.ends should equal (None)
        }
      }

      it("should set up a canceled active subscription to cancel immediately") {
        val now = (new DateTime())
        val nowSeconds = now.getMillis / 1000
        val nowFromSecs = new DateTime(nowSeconds * 1000)

        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "customer.subscription.updated") ~
          ("data" -> ("object" -> (
            ("customer" -> customer8.stripeCustomerId) ~
            ("plan" -> ("id" -> plan1.stripeId)) ~
            ("status" -> "canceled") ~
            ("ended_at" -> nowSeconds) ~
            ("cancel_at_period_end" -> false)
          )))

        okResponseTest(stripeData) { (emailActor) =>
          val user = User.find(customer8._id).get
          val subscription = user.subscriptions.head

          user.subscriptions should have length 1
          subscription.status should equal ("cancelled")
          subscription.ends.get should equal (nowFromSecs)
        }
      }

      it("should properly change plans if the users plan is switched") {
        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "customer.subscription.updated") ~
          ("data" -> ("object" -> (
            ("customer" -> customer9.stripeCustomerId) ~
            ("plan" -> ("id" -> plan2.stripeId)) ~
            ("status" -> "active") ~
            ("cancel_at_period_end" -> false)
          )))

        okResponseTest(stripeData) { (emailActor) =>
          val user = User.find(customer9._id).get
          val subscription = user.subscription.get

          user.subscriptions should have length 2
          subscription.status should equal ("active")
          subscription.planId should equal (plan2._id)
        }
      }
    }

    describe("customer.subscription.deleted") {
      it("should properly cancel a db subscription") {
        val tomorrow = (new DateTime()).plusDays(1)
        val tomorrowSeconds = tomorrow.getMillis / 1000
        val tomorrowFromSecs = new DateTime(tomorrowSeconds * 1000)

        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "customer.subscription.deleted") ~
          ("data" -> ("object" -> (
            ("customer" -> customer5.stripeCustomerId) ~
            ("status" -> "canceled") ~
            ("current_period_end" -> tomorrowSeconds)
          )))

        okResponseTest(stripeData) { (emailActor) =>
          val user = User.find(customer5._id).get
          val subscription = user.subscription.get

          user.subscriptions should have length 1
          subscription.status should equal ("cancelled")
          subscription.ends should equal (Some(tomorrowFromSecs))
        }
      }

      it("should immediately end a subscription that was unpaid") {
        val tomorrow = (new DateTime()).plusDays(1)
        val tomorrowSeconds = tomorrow.getMillis / 1000
        val tomorrowFromSecs = new DateTime(tomorrowSeconds * 1000)

        val stripeData =
          ("id" -> randomString(32)) ~
          ("type" -> "customer.subscription.deleted") ~
          ("data" -> ("object" -> (
            ("customer" -> customer6.stripeCustomerId) ~
            ("status" -> "unpaid") ~
            ("current_period_end" -> tomorrowSeconds)
          )))

        okResponseTest(stripeData) { (emailActor) =>
          val user = User.find(customer6._id).get
          val subscription = user.subscriptions.head

          user.subscriptions should have length 1
          subscription.status should equal ("stopped")
          subscription.ends.get.isBefore(new DateTime()) should equal (true)
          user.activeCard should equal (None)
        }
      }
    }
  }
}
