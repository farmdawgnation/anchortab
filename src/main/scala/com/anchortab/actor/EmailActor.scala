package com.anchortab.actor

import scala.xml.NodeSeq

import java.text.SimpleDateFormat

import net.liftweb._
  import common._
  import actor._
  import mongodb._
  import util.Helpers._
  import json._
    import ext._
    import JsonDSL._
    import Extraction._
  import util._
  import http._

import dispatch._
import com.ning.http.client.{Request, RequestBuilder, Response}

import org.joda.time._

import com.anchortab.model._

import org.bson.types.ObjectId

sealed trait EmailActorMessage
case class SendWelcomeEmail(userEmail: String) extends EmailActorMessage
case class SendForgotPasswordEmail(userEmail: String, resetLink: String) extends EmailActorMessage
case class SendQuotaWarningEmail(userEmail: String) extends EmailActorMessage
case class SendQuotaErrorEmail(userEmail: String) extends EmailActorMessage
case class SendTrialEndingEmail(userEmail: String, billingInfoPresent: Boolean, planName: String) extends EmailActorMessage
case class SendInvoicePaymentFailedEmail(userEmail: String, amount: Double, nextPaymentAttempt: Option[DateTime]) extends EmailActorMessage
case class SendInvoicePaymentSucceededEmail(userEmail: String, amount: Double) extends EmailActorMessage

object EmailActor extends LiftActor with Loggable {
  implicit val formats = DefaultFormats

  protected object Mandrill {
    private val key = "cpy4Hbh3VICrlVA5cpq_cw"
    private val endpointHost = "mandrillapp.com"
    private val endpointVersion = "1.0"

    case class MandrillTo(email: String, name: Option[String] = None)
    case class MandrillMessage(subject: String, from_email: String, to: List[MandrillTo],
      from_name: Option[String] = None, html: Option[String] = None,
      text: Option[String] = None)

    trait MandrillApiCall {
      def uri(requestBuilder: RequestBuilder): RequestBuilder
    }
    case class SendMandrillMessage(message: MandrillMessage, async: Boolean = false) extends MandrillApiCall {
      def uri(requestBuilder: RequestBuilder) = requestBuilder / "messages" / "send.json"
    }

    case class CodeResponse(code: Int)
    protected object AsCodeResponse extends (Response => CodeResponse) {
      def apply(r:Response) = {
        CodeResponse(
          r.getStatusCode()
        )
      }
    }

    case class MandrillResponse(status: String)

    def run(apiCall: MandrillApiCall) = {
      val postJson = decompose(apiCall) match {
        case obj:JObject =>
          obj ~
          ("key" -> key)

        case _ => JObject(Nil)
      }

      val requestBody = compact(render(postJson))
      val request = (apiCall.uri(host(endpointHost) / "api" / endpointVersion)).secure <<
        requestBody

      val response = Http(request > AsCodeResponse).either

      response() match {
        case Right(CodeResponse(200)) => // All good.
        case Right(CodeResponse(code)) => logger.error("Mandrill returned code: " + code)
        case Left(dispatchError) => logger.error("Dispatch error: " + dispatchError)
      }
    }
  }

  val fromEmail = "hello@anchortab.com"
  val fromName = "Anchor Tab"

  def sendEmail(subject: String, to: List[String], nodes: NodeSeq) = {
    val sendMandrillMessage = Mandrill.SendMandrillMessage(
      Mandrill.MandrillMessage(subject, fromEmail,
        to.map(Mandrill.MandrillTo(_)),
        Some(fromName),
        html = Some(nodes.toString))
    )

    Mandrill.run(sendMandrillMessage)
  }

  val welcomeEmailSubject = "Welcome to Anchor Tab!"
  val welcomeEmailTemplate = 
    Templates("emails-hidden" :: "welcome-email" :: Nil) openOr NodeSeq.Empty

  val forgotPasswordEmailTemplate =
    Templates("emails-hidden" :: "forgot-password-email" :: Nil) openOr NodeSeq.Empty

  val quotaWarningEmailTemplate =
    Templates("emails-hidden" :: "quota-warning-email" :: Nil) openOr NodeSeq.Empty

  val quotaErrorEmailTemplate =
    Templates("emails-hidden" :: "quota-error-email" :: Nil) openOr NodeSeq.Empty

  val trialEndingEmailTemplate =
    Templates("emails-hidden" :: "trial-ending-email" :: Nil) openOr NodeSeq.Empty

  val invoicePaymentFailedEmailTemplate =
    Templates("emails-hidden" :: "invoice-payment-failed-email" :: Nil) openOr NodeSeq.Empty

  def messageHandler = {
    case SendWelcomeEmail(userEmail) =>
      sendEmail(welcomeEmailSubject, userEmail :: Nil, welcomeEmailTemplate)

    case SendForgotPasswordEmail(userEmail, resetLink) =>
      val subject = "Anchor Tab Password Reset"

      val forgotPasswordMessage = (
        ".reset-link [href]" #> resetLink &
        ".reset-link *" #> resetLink
      ).apply(forgotPasswordEmailTemplate)

      sendEmail(subject, userEmail :: Nil, forgotPasswordMessage)

    case SendQuotaWarningEmail(userEmail) =>
      val subject = "Anchor Tab Quota Warning"

      sendEmail(subject, userEmail :: Nil, quotaWarningEmailTemplate)

    case SendQuotaErrorEmail(userEmail) =>
      val subject = "Anchor Tab Quota Error"

      sendEmail(subject, userEmail :: Nil, quotaErrorEmailTemplate)

    case SendTrialEndingEmail(userEmail, billingInfoPresent, planName) =>
      val subject = "Anchor Tab Trial Ending Soon"

      val trialEndingSoonMessage = (
        ".plan-name" #> planName andThen
        ".no-billing-info" #> (billingInfoPresent ? ClearNodes | PassThru) &
        ".billing-info" #> (billingInfoPresent ? PassThru | ClearNodes)
      ).apply(trialEndingEmailTemplate)

      sendEmail(subject, userEmail :: Nil, trialEndingSoonMessage)

    case SendInvoicePaymentFailedEmail(userEmail, amount, nextPaymentAttempt) =>
      val subject = "Problem Billing your Credit Card"
      val dateFormatter = new SimpleDateFormat("MMM dd")

      val invoicePaymentFailedMessage = (
        ".will-bill-again" #> (nextPaymentAttempt.isDefined ? PassThru | ClearNodes) andThen
        ".will-not-bill-again" #> (nextPaymentAttempt.isDefined ? ClearNodes | PassThru) andThen
        ".bill-amount" #> ("$" + ("%1.2f" format amount)) &
        ".next-payment-attempt" #> nextPaymentAttempt.map { paymentAttemptDate =>
          dateFormatter.format(paymentAttemptDate.toDate)
        }
      ).apply(invoicePaymentFailedEmailTemplate)

      sendEmail(subject, userEmail :: Nil, invoicePaymentFailedMessage)

    case _ =>
  }
}
