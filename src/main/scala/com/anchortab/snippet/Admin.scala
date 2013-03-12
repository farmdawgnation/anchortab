package com.anchortab.snippet

import scala.xml.NodeSeq

import java.text.SimpleDateFormat
import java.util.UUID

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
    import ext._
    import JsonDSL._
    import Extraction._
  import mongodb.BsonDSL._

import com.anchortab.model._

import com.stripe

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
    val requestPlan = requestPlanId.is.flatMap(Plan.find(_))

    var planName = requestPlan.map(_.name) openOr ""
    var planDescription = requestPlan.map(_.description) openOr ""
    var planTerm = requestPlan.map(_.term) openOr Plan.MonthlyTerm
    var planPrice = requestPlan.map(_.price) openOr 0.0
    var trialDays = requestPlan.map(_.trialDays) openOr 0
    var visible = requestPlan.map(_.visibleOnRegistration) openOr true
    var featureBasicAnalytics = requestPlan.map(_.hasFeature_?(Plan.Features.BasicAnalytics)) openOr false
    var featureWhitelabeledTabs = requestPlan.map(_.hasFeature_?(Plan.Features.WhitelabeledTabs)) openOr false
    var quotaNumberOfTabs = requestPlan.flatMap(_.quotaFor(Plan.Quotas.NumberOfTabs).map(_.toString)) openOr ""
    var quotaEmailSubscriptions = requestPlan.flatMap(_.quotaFor(Plan.Quotas.EmailSubscriptions).map(_.toString)) openOr ""
    var quotaViews = requestPlan.flatMap(_.quotaFor(Plan.Quotas.Views).map(_.toString)) openOr ""

    def submit() = {
      implicit val formats = DefaultFormats ++ JodaTimeSerializers.all

      val features = {
        Map(
          Plan.Features.BasicAnalytics -> featureBasicAnalytics,
          Plan.Features.WhitelabeledTabs -> featureWhitelabeledTabs
        )
      }

      val quotas = {
        def convertToQuotaBox(quotaName:String, input:String) = {
          (input.isEmpty ? Empty | tryo(input.toLong)).map(quotaName -> _)
        }

        val tabNumberQuota = convertToQuotaBox(Plan.Quotas.NumberOfTabs, quotaNumberOfTabs)
        val emailSubscriptionQuota = convertToQuotaBox(Plan.Quotas.EmailSubscriptions, quotaEmailSubscriptions)
        val viewQuota = convertToQuotaBox(Plan.Quotas.Views, quotaViews)

        val quotaList = tabNumberQuota :: emailSubscriptionQuota :: viewQuota :: Nil
        quotaList.foldLeft(Map[String, Long]())(_ ++ _)
      }

      requestPlanId.is match {
        case Empty =>
          val stripeId: Box[String] = {
            if (planPrice > 0) {
              val idString = "ANCHORTAB-" + UUID.randomUUID

              val planResult = tryo(stripe.Plan.create(Map(
                "amount" -> (planPrice * 100).toInt,
                "currency" -> "usd",
                "interval" -> planTerm.stripeCode,
                "name" -> planName,
                "trial_period_days" -> trialDays,
                "id" -> idString
              )))

              planResult.map(_ => idString)
            } else {
              Empty
            }
          }

          stripeId match {
            case fail:Failure =>
              Alert("Error from Stripe: " + fail.toString)

            case _ =>
              Plan( planName, planDescription, planPrice, trialDays,
                    features, quotas, visible, term = planTerm,
                    stripeId = stripeId).save

              RedirectTo("/admin/plans")
          }

        case Full(requestPlanId) =>
          val stripePlanIdUpdate = {
            if (requestPlan.map(_.price).map(_ != planPrice).openOr(false) ||
                requestPlan.map(_.term).map(_ != planTerm).openOr(false) ||
                requestPlan.map(_.name).map(_ != planName).openOr(false) ||
                requestPlan.map(_.trialDays).map(_ != trialDays).openOr(false)) {
              // Delete old plan, create new.
              requestPlan.flatMap(_.stripeId).foreach { stripeId =>
                tryo(stripe.Plan.retrieve(stripeId)).map { plan =>
                  plan.delete()
                }
              }

              val idString = "ANCHORTAB-" + UUID.randomUUID

              val planResult = tryo(stripe.Plan.create(Map(
                "amount" -> (planPrice * 100).toInt,
                "currency" -> "usd",
                "interval" -> planTerm.stripeCode,
                "name" -> planName,
                "trial_period_days" -> trialDays,
                "id" -> idString
              )))

              planResult.map(_ => idString)
            } else {
              // do nothing
              requestPlan.flatMap(_.stripeId)
            }
          }

          Plan.update("_id" -> new ObjectId(requestPlanId), "$set" -> (
            ("name" -> planName) ~
            ("description" -> planDescription) ~
            ("price" -> planPrice) ~
            ("features" -> features) ~
            ("quotas" -> quotas) ~
            ("visibleOnRegistration" -> visible) ~
            ("term" -> decompose(planTerm)) ~
            ("trialDays" -> trialDays) ~
            ("stripeId" -> stripePlanIdUpdate.toOption)
          ))

          RedirectTo("/admin/plans")

        case _ =>
          Alert("Something went wrong.")
      }
    }

    val bind =
      ".plan-name" #> text(planName, planName = _) &
      ".plan-description" #> text(planDescription, planDescription = _) &
      ".plan-price" #> text(planPrice.toString, (price) => planPrice = price.toDouble) &
      ".trial-days" #> text(trialDays.toString, (days) => trialDays = days.toInt) &
      ".plan-term" #> selectObj[PlanTerm](
        Plan.terms.map(t => (t, t.description.capitalize)),
        Full(planTerm),
        planTerm = _
      ) &
      ".plan-visible" #> checkbox(visible, visible = _) &
      ".feature-basic-analytics" #> checkbox(featureBasicAnalytics, featureBasicAnalytics = _) &
      ".feature-whitelabeled-tabs" #> checkbox(featureWhitelabeledTabs, featureWhitelabeledTabs = _) &
      ".quota-number-of-tabs" #> text(quotaNumberOfTabs, quotaNumberOfTabs = _) &
      ".quota-email-subscriptions" #> text(quotaEmailSubscriptions, quotaEmailSubscriptions = _) &
      ".quota-views" #> text(quotaViews, quotaViews = _) &
      ".submit" #> ajaxSubmit("Update Plan", submit _)

    "form" #> { ns:NodeSeq =>
      ajaxForm(bind(ns))
    }
  }

  def adminPlansList(xhtml:NodeSeq) = {
    val plans = Plan.findAll

    def editPlan(planId:ObjectId)(s:String) = {
      RedirectTo("/admin/plan/" + planId.toString + "/edit")
    }

    val planTransform =
      ".plan-row" #> plans.map { plan =>
        ".plan-name *" #> plan.name &
        ".plan-price *" #> plan.price.toString &
        ".plan-term *" #> plan.term.description &
        ".edit-plan [onclick]" #> onEvent(editPlan(plan._id) _)
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

    def impersonateUser(userId: ObjectId)(s: String) = {
      Authentication.impersonateUser(userId)
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
            ".impersonate-user [onclick]" #> onEvent(impersonateUser(user._id) _) &
            ".edit-user [onclick]" #> onEvent(editUser(user._id) _)
          }
        }
      }

    userListTransform.map(_.apply(xhtml)) openOr NodeSeq.Empty
  }
}
