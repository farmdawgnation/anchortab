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

import com.anchortab.model._

import org.bson.types.ObjectId

object requestUserId extends RequestVar[Box[String]](Empty)
object requestPlanId extends RequestVar[Box[String]](Empty)

object Admin {
  val shortDateFormatter = new SimpleDateFormat("MM/dd/yyyy")
  val longDateFormatter = new SimpleDateFormat("MMMM dd, yyyy")

  def statelessRewrite : RewritePF = {
    case RewriteRequest(ParsePath("admin" :: "user" :: userId :: "edit" :: Nil, _, _, _), _, _) =>
      requestUserId(Full(userId))
      RewriteResponse("admin" :: "user" :: "form" :: Nil)

    case RewriteRequest(ParsePath("admin" :: "users" :: "new" :: Nil, _, _, _), _, _) =>
      RewriteResponse("admin" :: "user" :: "form" :: Nil)

    case RewriteRequest(ParsePath("admin" :: "plan" :: planId :: "edit" :: Nil, _, _, _), _, _) =>
      requestPlanId(Full(planId))
      RewriteResponse("admin" :: "plan" :: "form" :: Nil)

    case RewriteRequest(ParsePath("admin" :: "plans" :: "new" :: Nil, _, _, _), _, _) =>
      RewriteResponse("admin" :: "plan" :: "form" :: Nil)
  }

  def snippetHandlers : SnippetPF = {
    case "admin-user-list" :: Nil => adminUserList _
    case "edit-user-form" :: Nil => editUserForm

    case "admin-plans-list" :: Nil => adminPlansList _
    case "edit-plan-form" :: Nil => editPlanForm
  }

  def editPlanForm = {
    var planName = ""
    var planDescription = ""
    var planTerm = ""
    var visible = false
    var featureBasicAnalytics = false
    var featureWhitelabeledTabs = false
    var quotaNumberOfTabs = ""
    var quotaEmailSubscriptions = ""
    var quotaViews = ""

    ".plan-name" #> text(planName, planName = _) &
    ".plan-description" #> text(planDescription, planDescription = _) &
    //".plan-term" #> select(
    //  
    //) &
    ".plan-visible" #> checkbox(visible, visible = _) &
    ".feature-basic-analytics" #> checkbox(featureBasicAnalytics, featureBasicAnalytics = _) &
    ".feature-whitelabeled-tabs" #> checkbox(featureWhitelabeledTabs, featureWhitelabeledTabs = _) &
    ".quota-number-of-tabs" #> text(quotaNumberOfTabs, quotaNumberOfTabs = _) &
    ".quota-email-subscriptions" #> text(quotaEmailSubscriptions, quotaEmailSubscriptions = _) &
    ".quota-views" #> text(quotaViews, quotaViews = _)
  }

  def adminPlansList(xhtml:NodeSeq) = {
    val plans = Plan.findAll

    val planTransform =
      ".plan-row" #> plans.map { plan =>
        ".plan-name *" #> plan.name &
        ".plan-price *" #> plan.price.toString &
        ".plan-term *" #> plan.term.description
      }

    planTransform.apply(xhtml)
  }

  def editUserForm = {
    {
      for {
        session <- userSession.is
        currentUser <- User.find(session.userId) if currentUser.admin_?
        user <- requestUserId.is.flatMap(User.find(_)) or Full(User("", ""))
      } yield {
        var firstName = user.profile.flatMap(_.firstName) getOrElse ""
        var lastName = user.profile.flatMap(_.lastName) getOrElse ""
        var organization = user.profile.flatMap(_.organization) getOrElse ""
        var email = user.email
        var changePassword = ""
        var confirmPassword = ""
        var isAdmin = false

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
                Full(User.hashPassword(p1))
            }
          }

          val userProfile = UserProfile(firstNameOpt, lastNameOpt, organizationOpt)

          (requestUserId.is, email, passwordChange) match {
            case (_, "", _) =>
              Alert("Email is a required field. It must have a value.")

            case (_, _, Failure(msg, _, _)) =>
              Alert(msg)

            case (Full(_), _, pw) =>
              User.update("_id" -> user._id, "$set" -> (
                ("email" -> email) ~
                ("profile" -> decompose(userProfile)) ~
                ("password" -> pw.toOption) ~
                ("role" -> (isAdmin ? "admin" | ""))
              ))

              RedirectTo("/admin/users")

            case (Empty, email, Full(pw)) =>
              if (isAdmin)
                User(email, pw, Some(userProfile), role = Some("admin")).save
              else
                User(email, pw, Some(userProfile)).save

              RedirectTo("/admin/users")

            case (Empty, _, Empty) =>
              Alert("Password is required for new users.")
          }
        }

        val adminCheckboxAttrs =
          if (currentUser._id.toString == user._id.toString)
            ("disabled" -> "disabled")
          else
            ("class" -> "admin-checkbox")

        val bind =
          ".first-name" #> text(firstName, firstName = _) &
          ".last-name" #> text(lastName, lastName = _) &
          ".organization" #> text(organization, organization = _) &
          ".email" #> text(email, email = _) &
          ".change-password" #> password(changePassword, changePassword = _) &
          ".confirm-password" #> password(confirmPassword, confirmPassword = _) &
          ".admin-checkbox" #> checkbox(user.admin_?, isAdmin = _, adminCheckboxAttrs) &
          ".submit" #> ajaxSubmit("Update Profile", submit _)

        "form" #> { ns:NodeSeq =>
          ajaxForm(bind(ns))
        }
      }
    } openOr {
      "form" #> ClearNodes
    }
  }

  def adminUserList(xhtml:NodeSeq) = {
    def editUser(userId:ObjectId)(s:String) = {
      RedirectTo("/admin/user/" + userId.toString + "/edit")
    }

    val userListTransform =
      {
        for {
          session <- userSession.is
          currentUser <- User.find(session.userId) if currentUser.admin_?
          users = User.findAll
        } yield {
          ".user-row" #> users.map { user =>
            ".email *" #> user.email &
            ".name *" #> user.name &
            ".edit-user [onclick]" #> onEvent(editUser(user._id) _)
          }
        }
      }

    userListTransform.map(_.apply(xhtml)) openOr NodeSeq.Empty
  }
}
