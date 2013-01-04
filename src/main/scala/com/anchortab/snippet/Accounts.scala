package com.anchortab.snippet

import scala.xml.NodeSeq

import java.text.SimpleDateFormat

import net.liftweb._
  import http._
    import js._
      import JsCmds._
    import SHtml._
    import LiftRules._
  import common._
  import util._
    import Helpers._
  import json._
    import JsonDSL._
    import Extraction._
  import mongodb.BsonDSL._

import com.anchortab.model.{User, UserProfile}
import com.anchortab.constantcontact.ConstantContact

import org.bson.types.ObjectId

object Accounts {
  val shortDateFormatter = new SimpleDateFormat("MM/dd/yyyy")
  val longDateFormatter = new SimpleDateFormat("MMMM dd, yyyy")

  def snippetHandlers : SnippetPF = {
    case "profile-form" :: Nil => profileForm
    case "new-oauth-accounts" :: Nil => newOAuthAccounts
    case "user-service-credentials" :: Nil => userServiceCredentials
  }

  def userServiceCredentials = {
    {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
      } yield {
        ".service-credential" #> user.serviceCredentials.map { serviceCredential =>
          ".service-name *" #> serviceCredential.serviceName &
          ".service-user-identifier *" #> serviceCredential.userIdentifier
        }
      }
    } openOr {
      "form" #> ClearNodes
    }
  }

  def newOAuthAccounts = {
    ".constantcontact [href]" #> ConstantContact.oauthAuthorizeUrl &
    ".mailchimp [href]" #> ""
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

          val userProfile = UserProfile(firstNameOpt, lastNameOpt, organizationOpt)

          (email, passwordChange) match {
            case ("", _) =>
              Alert("Email is a required field. It must have a value.")

            case (_, Failure(msg, _, _)) =>
              Alert(msg)

            case (_, Empty) =>
              User.update("_id" -> user._id, "$set" -> (
                ("email" -> email) ~
                ("profile" -> decompose(userProfile))
              ))

              Alert("Profile updated.")

            case (_, Full(pw)) =>
              User.update("_id" -> user._id, "$set" -> (
                ("email" -> email) ~
                ("profile" -> decompose(userProfile)) ~
                pw
              ))

              Alert("Profile updated.")
          }
        }

        val bind =
          ".first-name" #> text(firstName, firstName = _) &
          ".last-name" #> text(lastName, lastName = _) &
          ".organization" #> text(organization, organization = _) &
          ".email" #> text(email, email = _) &
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
