package com.anchortab.actor

import scala.xml.NodeSeq
import scala.util._

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
import com.anchortab.util._

import com.stripe

import org.bson.types.ObjectId

sealed trait AdminNotificationType
case object SubscriptionDeleted extends AdminNotificationType
case class PlanChanged(oldPlanId: ObjectId, newPlanId: ObjectId) extends AdminNotificationType

sealed trait EmailActorMessage
case class SendAdminNotificationEmail(notificationType: AdminNotificationType, userEmail: String) extends EmailActorMessage
case class SendWelcomeEmail(userEmail: String) extends EmailActorMessage
case class SendForgotPasswordEmail(userEmail: String, resetLink: String) extends EmailActorMessage
case class SendQuotaWarningEmail(userEmail: String) extends EmailActorMessage
case class SendQuotaErrorEmail(userEmail: String) extends EmailActorMessage
case class SendTrialEndingEmail(userEmail: String, billingInfoPresent: Boolean, planName: String, trialEnd: DateTime) extends EmailActorMessage
case class SendInvoicePaymentFailedEmail(userEmail: String, amount: Double, nextPaymentAttempt: Option[DateTime]) extends EmailActorMessage
case class SendInvoicePaymentSucceededEmail(user: User, invoice: stripe.Invoice) extends EmailActorMessage
case class SendNeighborhoodWatchEmail(
  sameSiteMultipleAccount: List[SameSiteMultipleAccount],
  multipleAccountsSameIpAndUserAgent: List[MultipleAccountsSameIp],
  similarEmailAddresses: List[SimilarEmailAddresses]
) extends EmailActorMessage
case class SendLeadGenerationSubscriptionEmail(targetEmail: String, tabName: String, subscribedEmail: String, subscriberName: Option[String]) extends EmailActorMessage
case class SendSubmitErrorNotificationEmail(targetEmail: String, erroredTabs: List[Tab]) extends EmailActorMessage
case class SendRetentionEmail(targetEmail: String) extends EmailActorMessage
case class SendMassEmail(emailType: AnchorTabEmailType, subject: String, message: NodeSeq) extends EmailActorMessage

trait AdminNotificationEmailHandling extends EmailHandlerChain {
  val adminNotificationTemplate = Templates("emails-hidden" :: "admin-notification-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendAdminNotificationEmail(notificationType, userEmail) =>
      val eventType = notificationType match {
        case SubscriptionDeleted =>
          "Subscription deleted."

        case PlanChanged(oldPlanId, newPlanId) =>
          val oldPlan = Plan.find(oldPlanId).map(_.name) getOrElse "N/A"
          val newPlan = Plan.find(newPlanId).map(_.name) getOrElse "N/A"

          s"Plan changed from $oldPlan to $newPlan."
      }

      val adminNotificationMessage = (
        ".user-email *" #> userEmail &
        ".event-type *" #> eventType
      ).apply(adminNotificationTemplate)

      val admins = User.findAll("role" -> User.Roles.Admin).map(_.email)

      sendEmail("Anchor Tab Admin Notification", admins, adminNotificationMessage, AdminEmail)
  }
}

trait WelcomeEmailHandling extends EmailHandlerChain {
  val welcomeEmailSubject = "Welcome to Anchor Tab!"
  val welcomeEmailTemplate = 
    Templates("emails-hidden" :: "welcome-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendWelcomeEmail(userEmail) =>
      sendEmail(welcomeEmailSubject, userEmail :: Nil, welcomeEmailTemplate, WelcomeEmail)
  }
}

trait ForgotPasswordEmailHandling extends EmailHandlerChain {
  val forgotPasswordEmailTemplate =
    Templates("emails-hidden" :: "forgot-password-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendForgotPasswordEmail(userEmail, resetLink) =>
      val subject = "Anchor Tab Password Reset"

      val forgotPasswordMessage = (
        ".reset-link [href]" #> resetLink &
        ".reset-link *" #> resetLink
      ).apply(forgotPasswordEmailTemplate)

      sendEmail(subject, userEmail :: Nil, forgotPasswordMessage, ForgotPasswordEmail)
  }
}

trait QuotaWarningEmailHandling extends EmailHandlerChain {
  val quotaWarningEmailTemplate =
    Templates("emails-hidden" :: "quota-warning-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendQuotaWarningEmail(userEmail) =>
      val subject = "Anchor Tab Quota Warning"

      sendEmail(subject, userEmail :: Nil, quotaWarningEmailTemplate, AlertEmail)
  }
}

trait QuotaErrorEmailHandling extends EmailHandlerChain {
  val quotaErrorEmailTemplate =
    Templates("emails-hidden" :: "quota-error-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendQuotaErrorEmail(userEmail) =>
      val subject = "Anchor Tab Quota Error"

      sendEmail(subject, userEmail :: Nil, quotaErrorEmailTemplate, AlertEmail)
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

      sendEmail(subject, userEmail :: Nil, trialEndingSoonMessage, BillingAlert)
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

      sendEmail(subject, userEmail :: Nil, invoicePaymentFailedMessage, BillingAlert)
  }
}

trait InvoicePaymentSucceededEmailHandling extends EmailHandlerChain with StripeInvoiceRendering {
  val invoicePaymentSucceededEmailTemplate =
    Templates("emails-hidden" :: "invoice-payment-succeeded-email" :: Nil) openOr NodeSeq.Empty

  addHandler {
    case SendInvoicePaymentSucceededEmail(user, invoice) if user.notificationSettings.emailReceipts =>
      val subject = "Anchor Tab Receipt"
      val invoicePaymentSucceededMessage = renderInvoice(user, invoice).apply(invoicePaymentSucceededEmailTemplate)
      sendEmail(subject, user.email :: Nil, invoicePaymentSucceededMessage, ReceiptEmail)

    case _: SendInvoicePaymentSucceededEmail =>
      // User has elected not to receive receipts.
  }
}

trait NeighborhoodWatchEmailHandling extends EmailHandlerChain {
  val template =
    Templates("emails-hidden" :: "neighborhood-watch-email" :: Nil) openOr NodeSeq.Empty
  val subject = "Anchor Tab Neighborhood Watch"
  val to = "main+neighborhoodwatch@anchortab.flowdock.com"

  addHandler {
    case SendNeighborhoodWatchEmail(sameSiteMultipleAccount, multipleAccountsSameIpAndUserAgent, similarEmailAddresses) =>
      val allClear =
        sameSiteMultipleAccount.isEmpty &&
        multipleAccountsSameIpAndUserAgent.isEmpty &&
        similarEmailAddresses.isEmpty

      val neighborhoodWatchMessage = (
        ".all-clear" #> (allClear ? PassThru | ClearNodes) andThen
        ".intro" #> (allClear ? ClearNodes | PassThru) andThen
        ".same-site-multiple-account" #> (sameSiteMultipleAccount.nonEmpty ? PassThru | ClearNodes) andThen
        ".same-site-multiple-account-item" #> sameSiteMultipleAccount.map { result =>
          ".domain *" #> result.domain &
          ".accounts *" #> result.accounts.mkString(", ")
        } &
        ".multiple-accounts-same-ip-and-user-agent" #> (multipleAccountsSameIpAndUserAgent.nonEmpty ? PassThru | ClearNodes) andThen
        ".multiple-accounts-same-ip-and-user-agent-item" #> multipleAccountsSameIpAndUserAgent.map { result =>
          ".accounts *" #> result.accounts.mkString(", ") &
          ".ip-address *" #> result.ip
        } &
        ".similar-email-registration" #> (similarEmailAddresses.nonEmpty ? PassThru | ClearNodes) andThen
        ".similar-email-address-item" #> similarEmailAddresses.map { result =>
          ".accounts *" #> result.accounts.mkString(", ")
        }
      ).apply(template)

      sendEmail(subject, to :: Nil, neighborhoodWatchMessage, AdminEmail)
  }
}

trait LeadGenerationSubscriptionEmailHandling extends EmailHandlerChain {
  val leadGenerationTemplate =
    Templates("emails-hidden" :: "lead-generation-subscription-email" :: Nil) openOr NodeSeq.Empty
  val leadGenerationSubject = "New Lead from your Anchor Tab"

  addHandler {
    case SendLeadGenerationSubscriptionEmail(targetEmail, tabName, subscribedEmail, subscriberName) =>
      val transform =
        ".email-address [href]" #> ("mailto:" + subscribedEmail) &
        ".tab-name *" #> tabName &
        ".email-address *" #> subscribedEmail &
        ".subscriber-name-line" #> subscriberName.map { name =>
          ".subscriber-name *" #> name
        }

      val message = transform.apply(leadGenerationTemplate)

      sendEmail(leadGenerationSubject, targetEmail :: Nil, message, LeadGenerationEmail)
  }
}

trait SubmitErrorNotificationEmailHandling extends EmailHandlerChain {
  val submitErrorNotificationTemplate =
    Templates("emails-hidden" :: "submit-error-notification-email" :: Nil) openOr NodeSeq.Empty
  val submitErrorNotificationSubject = "Action Required: Error submitting emails."

  case class ErrorDescriptor(email: String, formattedTime: String, tabName: String, error: String)

  addHandler {
    case SendSubmitErrorNotificationEmail(targetEmail, erroredTabs) =>
      val yesterday = (new DateTime()).withTimeAtStartOfDay().minusDays(1)

      val errorDescriptors =
        for {
          tab <- erroredTabs
          tabError <- tab.errors.filter(_.createdAt isAfter yesterday)
        } yield {
          ErrorDescriptor(
            tabError.email,
            tabError.createdAt.toString("MM/dd/yyyy hh:mm aa") + " UTC",
            tab.name,
            tabError.message
          )
        }

      val transform = ".error" #> errorDescriptors.map { errorDescriptor =>
        ".email *" #> errorDescriptor.email &
        ".time *" #> errorDescriptor.formattedTime &
        ".tab-name *" #> errorDescriptor.tabName &
        ".error-message *" #> errorDescriptor.error
      }

      val message = transform.apply(submitErrorNotificationTemplate)

      sendEmail(submitErrorNotificationSubject, targetEmail :: Nil, message, AlertEmail)
  }
}

trait RetentionEmailHandling extends EmailHandlerChain {
  val retentionEmailTemplate =
    Templates("emails-hidden" :: "retention-email" :: Nil) openOr NodeSeq.Empty
  val retentionEmailSubject = "How can we help?"

  addHandler {
    case SendRetentionEmail(targetEmail) =>
      sendEmail(retentionEmailSubject, targetEmail :: Nil, retentionEmailTemplate, WelcomeEmail)
  }
}

trait MassEmailHandling extends EmailHandlerChain {
  addHandler {
    case SendMassEmail(emailType, subject, message) =>
      val users = emailType match {
        case UrgentNotice =>
          User.findAll

        case AlertEmail =>
          User.findAll("notificationSettings.alertEmails" -> true)

        case _ =>
          User.findAll("notificationSettings.announcementEmails" -> true)
      }

      sendEmail(subject, users.map(_.email), message, emailType, true)
  }
}

sealed trait AnchorTabEmailType {
  def listDescription: String
}
case object AdminEmail extends AnchorTabEmailType {
  val listDescription = "This is an email dispatched to admins."
}
case object ForgotPasswordEmail extends AnchorTabEmailType {
  val listDescription = "This is a forgot password email triggered from the Anchor Tab website. If you did not trigger this email, please contact us immediately."
}
case object LeadGenerationEmail extends AnchorTabEmailType {
  val listDescription = "You are receiving this email because one of your tabs is running in Lead Generation mode."
}
case object WelcomeEmail extends AnchorTabEmailType {
  val listDescription = "This is a welcome email associated with your recently created Anchor Tab account. If you did not create an Anchor Tab account or don't know what this is, hit reply and let us know."
}
case object AnnouncementEmail extends AnchorTabEmailType {
  val listDescription = "You are receiving this email because Anchor Tab Announcement emails are enabled on your Anchor Tab account. You can disable these emails by turning off Announcement Emails in your profile."
}
case object AlertEmail extends AnchorTabEmailType {
  val listDescription = "You are receiving this email because Anchor Tab alert emails, which include emails about scheduled downtime and your tab quotas, are turned on for your account. You can disable these emails by turning off Alert Emails in your profile."
}
case object ReceiptEmail extends AnchorTabEmailType {
  val listDescription = "You are receiving this email because you have requested we send you Email Receipts. You can disable these emails by turning off Email Receipts in your profile."
}
case object BillingAlert extends AnchorTabEmailType {
  val listDescription = "You are receiving this email because we're having trouble billing your account for your Anchor Tab subscription."
}
case object UrgentNotice extends AnchorTabEmailType {
  val listDescription = "You are receiving this email because it is an urgent notice related to security, Terms of Service changes, or other matters that we always inform all Anchor Tab users about."
}

object EmailActor extends EmailActor
trait EmailActor extends EmailHandlerChain
                    with WelcomeEmailHandling
                    with ForgotPasswordEmailHandling
                    with QuotaWarningEmailHandling
                    with QuotaErrorEmailHandling
                    with TrialEndingEmailHandling
                    with InvoicePaymentFailedEmailHandling
                    with InvoicePaymentSucceededEmailHandling
                    with NeighborhoodWatchEmailHandling
                    with LeadGenerationSubscriptionEmailHandling
                    with SubmitErrorNotificationEmailHandling
                    with RetentionEmailHandling
                    with MassEmailHandling
                    with AdminNotificationEmailHandling {
  implicit val formats = DefaultFormats

  val fromEmail = "hello@anchortab.com"
  val fromName = "Anchor Tab"

  def sendEmail(
    subject: String,
    to: List[String],
    nodes: NodeSeq,
    emailType: AnchorTabEmailType,
    bccEmails: Boolean = false
  ) = {
    if (Props.productionMode || Properties.envOrNone("SEND_EMAILS").isDefined) {
      val mandrillDestinationInformation = if (bccEmails) {
        val mandrillBccs = to.map(Mandrill.MandrillTo(_, None, "bcc"))
        val anchorTabTo = Mandrill.MandrillTo("blackhole@anchortab.com", Some("Anchor Tab Users"), "to")

        anchorTabTo +: mandrillBccs
      } else {
        to.map(Mandrill.MandrillTo(_))
      }

      val sendMandrillMessage = Mandrill.SendTemplateMandrillMessage(
        Mandrill.MandrillMessage(
          subject, fromEmail,
          mandrillDestinationInformation,
          Some(fromName)
        ),
        "Anchor Tab Single Column",
        List(
          Map("name" -> "messageContent", "content" -> nodes.toString),
          Map("name" -> "listDescription", "content" -> emailType.listDescription)
        )
      )

      Mandrill.run(sendMandrillMessage)
    } else {
      logger.info(nodes.text)
    }
  }
}
