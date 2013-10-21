package com.anchortab.snippet

import scala.xml.NodeSeq

import java.text.SimpleDateFormat

import scala.collection.JavaConversions._

import net.liftweb._
  import sitemap._
  import http._
    import js._
      import JsCmds._
    import SHtml._
    import LiftRules._
  import common._
  import util._
    import Helpers._
  import json._
    import Extraction._
  import mongodb.BsonDSL._

import com.anchortab.model.{User, UserProfile}
import com.anchortab.constantcontact.ConstantContact
import com.anchortab.mailchimp._
import com.anchortab.campaignmonitor._

import com.stripe

import org.bson.types.ObjectId

import com.newrelic.api.agent.NewRelic

object Accounts extends Loggable {
  val profileMenu = Menu.i("Profile") / "manager" / "account"
  val servicesMenu = Menu.i("Connected Services") / "manager" / "services"

  val menus =
    profileMenu ::
    servicesMenu ::
    Nil

  val shortDateFormatter = new SimpleDateFormat("MM/dd/yyyy")
  val longDateFormatter = new SimpleDateFormat("MMMM dd, yyyy")

  def snippetHandlers : SnippetPF = {
    case "profile-form" :: Nil => profileForm
    case "campaign-monitor-connection" :: Nil => campaignMonitorConnection _
    case "constant-contact-connection" :: Nil => constantContactConnection _
    case "mailchimp-connection" :: Nil => mailchimpConnection _
  }

  def mailchimpConnection(xhtml:NodeSeq) = {
    def startMailchimpOAuth(s: String) = {
      RedirectTo(MailchimpOAuth.oAuthAuthorizeUrl)
    }

    def disconnectMailchimpOAuth(s: String) = {
      {
        for {
          session <- userSession.is
        } yield {
          User.update("_id" -> session.userId, "$pull" -> ("serviceCredentials" -> ("serviceName" -> "Mailchimp")))
          Notices.notice("Your MailChimp account has been disconnected.")
          Reload
        }
      } openOr {
        GeneralError("Something went wrong.")
      }
    }

    val connectionTransform =
      {
        for {
          session <- userSession.is
          user <- User.find(session.userId)
          credentials <- user.credentialsFor("Mailchimp")
          username = credentials.userIdentifier
        } yield {
          ".connection-status *" #> ("Connected to " + username + ".") &
          ".connect-service" #> ClearNodes &
          ".disconnect-service [onclick]" #> onEvent(disconnectMailchimpOAuth _)
        }
      } openOr {
        ".disconnect-service" #> ClearNodes &
        ".connect-service [onclick]" #> onEvent(startMailchimpOAuth _)
      }

    connectionTransform.apply(xhtml)
  }

  def constantContactConnection(xhtml:NodeSeq) = {
    def startConstantContactOauth(s:String) = {
      RedirectTo(ConstantContact.oauthAuthorizeUrl)
    }

    def disconnectConstantContactOauth(s:String) = {
      {
        for {
          session <- userSession.is
        } yield {
          User.update("_id" -> session.userId, "$pull" -> ("serviceCredentials" -> ("serviceName" -> "Constant Contact")))
          Notices.notice("Your Constant Contact account has been disconnected.")
          Reload
        }
      } openOr {
        GeneralError("Something went wrong.")
      }
    }

    val connectionTransform =
      {
        for {
          session <- userSession.is
          user <- User.find(session.userId)
          credentials <- user.credentialsFor("Constant Contact")
          username = credentials.userIdentifier
        } yield {
          ".connection-status *" #> ("Connected to " + username + ".") &
          ".connect-service" #> ClearNodes &
          ".disconnect-service [onclick]" #> onEvent(disconnectConstantContactOauth _)
        }
      } openOr {
        ".disconnect-service" #> ClearNodes &
        ".connect-service [onclick]" #> onEvent(startConstantContactOauth _)
      }

    connectionTransform.apply(xhtml)
  }

  def campaignMonitorConnection(xhtml:NodeSeq) = {
    def startCampaignMonitorOauth(s:String) = {
      {
        for (oauthUrl <- CampaignMonitor.oauthAuthorizeUrl)  yield {
          RedirectTo(oauthUrl)
        }
      } match {
        case Full(jsCmd) => jsCmd
        case somethingUnexpected =>
          logger.error("Error calculating Campaign Monitor OAuth URL: " + somethingUnexpected)
          GeneralError("Something went wrong.")
      }
    }

    def disconnectCampaignMonitorOauth(s:String) = {
      {
        for {
          session <- userSession.is
        } yield {
          User.update("_id" -> session.userId, "$pull" -> ("serviceCredentials" -> ("serviceName" -> CampaignMonitor.serviceIdentifier)))
          Notices.notice("Your Campaign Monitor account has been disconnected.")
          Reload
        }
      } openOr {
        GeneralError("Something went wrong.")
      }
    }

    val connectionTransform =
      {
        for {
          session <- userSession.is
          user <- User.find(session.userId)
          credentials <- user.credentialsFor(CampaignMonitor.serviceIdentifier)
          username = credentials.userIdentifier
        } yield {
          ".connection-status *" #> ("Connected.") &
          ".connect-service" #> ClearNodes &
          ".disconnect-service [onclick]" #> onEvent(disconnectCampaignMonitorOauth _)
        }
      } openOr {
        ".disconnect-service" #> ClearNodes &
        ".connect-service [onclick]" #> onEvent(startCampaignMonitorOauth _)
      }

    connectionTransform.apply(xhtml)
  }

  def profileForm = {
    {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
      } yield {
        var firstName = user.profile.flatMap(_.firstName) getOrElse ""
        var lastName = user.profile.flatMap(_.lastName) getOrElse ""
        var organization = user.profile.flatMap(_.organization) getOrElse ""
        var email = user.email
        var changePassword = ""
        var confirmPassword = ""

        def submit() = {
          implicit val formats = DefaultFormats

          val firstNameOpt = {
            firstName match {
              case "" => None
              case s => Some(s)
            }
          }
          val lastNameOpt = {
            lastName match {
              case "" => None
              case s => Some(s)
            }
          }
          val organizationOpt = {
            organization match {
              case "" => None
              case s => Some(s)
            }
          }

          val passwordChange = {
            (changePassword, confirmPassword) match {
              case ("", "") => Empty

              case (p1, p2) if p1 != p2 =>
                Failure("The passwords you selected do not match.")

              case (p1, p2) =>
                Full(("password" -> User.hashPassword(p1)))
            }
          }

          val emailChange = {
            for {
              noOtherUsersHaveEmail <- Full(User.countOfUsersWithEmail(email)).filter(_ == 0 || email == user.email) ?~ "Another user already has this email."
              stripeCustomerId <- (user.stripeCustomerId: Box[String]) ?~ "Stripe token missing."
                if user.email != email && email != ""
              stripeCustomer <- tryo(stripe.Customer.retrieve(stripeCustomerId))
              updateResult <- tryo(stripeCustomer.update(Map(
                "email" -> email
              )))
            } yield {
              true
            }
          }

          val fatalEmailChangeError = emailChange match {
            case Failure(msg, _, _) if msg == "Another user already has this email." =>
              true

            case Failure(msg, _, _) =>
              NewRelic.noticeError("Email Change Stripe Error", Map(
                "user" -> user._id.toString,
                "error" -> msg
              ))

              Notices.warning(msg)

              false

            case _ =>
              false
          }

          val userProfile = UserProfile(firstNameOpt, lastNameOpt, organizationOpt)

          (fatalEmailChangeError, email, passwordChange) match {
            case (true, _, _) =>
              FormValidationError(".email", "Another user already has this email.")

            case (_, "", _) =>
              FormValidationError(".email", "Email is a required field.")

            case (_, _, Failure(msg, _, _)) =>
              FormValidationError(".change-password", "") &
              FormValidationError(".confirm-password", msg)

            case (_, _, Empty) =>
              User.update("_id" -> user._id, "$set" -> (
                ("email" -> email) ~
                ("profile" -> decompose(userProfile))
              ))

              Notices.notice("Your profile was updated successfully. Jolly good fun.")
              Reload

            case (_, _, Full(pw)) =>
              User.update("_id" -> user._id, "$set" -> (
                ("email" -> email) ~
                ("profile" -> decompose(userProfile)) ~
                pw
              ))

              Notices.notice("Your profile and password were updated. A password change a day keeps the hacker away.")
              Reload
          }
        }

        val bind =
          ".first-name" #> text(firstName, firstName = _) &
          ".last-name" #> text(lastName, lastName = _) &
          ".organization" #> text(organization, organization = _) &
          ".email" #> text(email, newEmail => email = newEmail.trim) &
          ".change-password" #> password(changePassword, changePassword = _) &
          ".confirm-password" #> password(confirmPassword, confirmPassword = _) &
          ".submit" #> ajaxSubmit("Update Profile", submit _)

        "form" #> { ns:NodeSeq =>
          ajaxForm(bind(ns))
        }
      }
    } openOr {
      "form" #> ClearNodes
    }
  }
}
