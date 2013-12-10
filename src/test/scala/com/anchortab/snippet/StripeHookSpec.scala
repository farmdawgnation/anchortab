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

import org.joda.time._

object StripeHookSpecExamples {
  def customer(activeCard: Boolean = false) = {
    var user = User(
      email = randomString(32),
      password = randomString(32),
      stripeCustomerId = Some(randomString(32))
    )

    if (activeCard)
      user = user.copy(activeCard = Some(UserActiveCard("1234", "mc", 4, 2013)))

    user.save

    user
  }

  lazy val customer1 = customer()
  lazy val customer2 = customer(activeCard = true)
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
  }

  override def afterAll() {
    customer1.delete
    customer2.delete
  }

  describe("POST /stripe-hook") {
    describe("general processing") {
      it("should return 500 if the request body isn't valid JSON")(pending)

      it("should return a 404 on attempting to process an event id twice")(pending)

      it("should return a 500 if the event type is missing")(pending)

      it("should return a 500 if the data attribute is missing")(pending)

      it("should return a 500 if the data object attribute is missing")(pending)
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

        withStripeHookAndEmailActor { (stripeHook, emailActor) =>
          runStripeHookRequest(stripeHook, stripeData) { response =>
            response match {
              case Full(response) =>
                response should equal (OkResponse())

                val expectedEmailActorMessage = SendInvoicePaymentSucceededEmail(customer1.email.toLowerCase, 100.0)
                emailActor.messages should contain (expectedEmailActorMessage)

              case other =>
                fail("got: " + other)
            }
          }
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

        withStripeHookAndEmailActor { (stripeHook, emailActor) =>
          runStripeHookRequest(stripeHook, stripeData) { response =>
            response match {
              case Full(response) =>
                response should equal (OkResponse())

                val expectedEmailActorMessage = SendInvoicePaymentFailedEmail(
                  customer2.email.toLowerCase,
                  100.0,
                  Some(tomorrowFromSections)
                )
                emailActor.messages should contain (expectedEmailActorMessage)

              case other =>
                fail("got: " + other)
            }
          }
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

        withStripeHookAndEmailActor { (stripeHook, emailActor) =>
          runStripeHookRequest(stripeHook, stripeData) { response =>
            response match {
              case Full(response) =>
                response should equal (OkResponse())

                val expectedEmailActorMessage = SendInvoicePaymentFailedEmail(
                  customer2.email.toLowerCase,
                  100.0,
                  None
                )
                emailActor.messages should contain (expectedEmailActorMessage)

              case other =>
                fail("got: " + other)
            }
          }
        }
      }
    }

    describe("customer.subscription.created") {
      it("should create a new plan from stripe for a new subscription")(pending)
    }

    describe("customer.subscription.updated") {
      it("should set up a canceled subscription to cancel")(pending)

      it("should properly change plans if the users plan is switched")(pending)
    }

    describe("customer.subscription.deleted") {
      it("should properly cancel a db subscription")(pending)

      it("should immediately end a subscription that was unpaid")(pending)
    }

    describe("customer.subscription.trial_will_end") {
      it("should send an email to a user with an ending trial")(pending)
    }
  }
}
