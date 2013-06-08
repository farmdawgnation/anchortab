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
import com.anchortab.lib._

import org.bson.types.ObjectId

sealed trait EmailActorMessage
case class SendWelcomeEmail(userEmail: String) extends EmailActorMessage
case class SendForgotPasswordEmail(userEmail: String, resetLink: String) extends EmailActorMessage
case class SendQuotaWarningEmail(userEmail: String) extends EmailActorMessage
case class SendQuotaErrorEmail(userEmail: String) extends EmailActorMessage
case class SendTrialEndingEmail(userEmail: String, billingInfoPresent: Boolean, planName: String, trialEnd: DateTime) extends EmailActorMessage
case class SendInvoicePaymentFailedEmail(userEmail: String, amount: Double, nextPaymentAttempt: Option[DateTime]) extends EmailActorMessage
case class SendInvoicePaymentSucceededEmail(userEmail: String, amount: Double) extends EmailActorMessage

trait WelcomeEmailHandling extends EmailHandlerChain {
  val welcomeEmailSubject = "Welcome to Anchor Tab!"
  val welcomeEmailTemplate = 
    Templates("emails-hidden" :: "welcome-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendWelcomeEmail(userEmail) =>
      sendEmail(welcomeEmailSubject, userEmail :: Nil, welcomeEmailTemplate)
  }
}

trait ForgotPasswordEmailHandling extends EmailHandlerChain {
  val forgotPasswordEmailTemplate =
    Templates("emails-hidden" :: "forgot-password-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendForgotPasswordEmail(userEmail, resetLink) =>
      println("I FOTGOT A PASSWURD? WUT?")
      val subject = "Anchor Tab Password Reset"

      val forgotPasswordMessage = (
        ".reset-link [href]" #> resetLink &
        ".reset-link *" #> resetLink
      ).apply(forgotPasswordEmailTemplate)

      sendEmail(subject, userEmail :: Nil, forgotPasswordMessage)
  }
}

trait QuotaWarningEmailHandling extends EmailHandlerChain {
  val quotaWarningEmailTemplate =
    Templates("emails-hidden" :: "quota-warning-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendQuotaWarningEmail(userEmail) =>
      val subject = "Anchor Tab Quota Warning"

      sendEmail(subject, userEmail :: Nil, quotaWarningEmailTemplate)
  }
}

trait QuotaErrorEmailHandling extends EmailHandlerChain {
  val quotaErrorEmailTemplate =
    Templates("emails-hidden" :: "quota-error-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendQuotaErrorEmail(userEmail) =>
      val subject = "Anchor Tab Quota Error"

      sendEmail(subject, userEmail :: Nil, quotaErrorEmailTemplate)
  }
}

trait TrialEndingEmailHandling extends EmailHandlerChain {
  val trialEndingEmailTemplate =
    Templates("emails-hidden" :: "trial-ending-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendTrialEndingEmail(userEmail, billingInfoPresent, planName, trialEnd) =>
      val subject = "Anchor Tab Trial Ending Soon"

      val trialEndingSoonMessage = (
        ".trial-end-date" #> trialEnd.toString("dd MMM yyyy") &
        ".plan-name" #> planName andThen
        ".no-billing-info" #> (billingInfoPresent ? ClearNodes | PassThru) &
        ".billing-info" #> (billingInfoPresent ? PassThru | ClearNodes)
      ).apply(trialEndingEmailTemplate)

      sendEmail(subject, userEmail :: Nil, trialEndingSoonMessage)
  }
}

trait InvoicePaymentFailedEmailHandling extends EmailHandlerChain {
  val invoicePaymentFailedEmailTemplate =
    Templates("emails-hidden" :: "invoice-payment-failed-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
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
  }
}

object EmailActor extends EmailHandlerChain
                  with WelcomeEmailHandling
                  with ForgotPasswordEmailHandling
                  with QuotaWarningEmailHandling
                  with QuotaErrorEmailHandling
                  with TrialEndingEmailHandling
                  with InvoicePaymentFailedEmailHandling {
  implicit val formats = DefaultFormats

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
}
