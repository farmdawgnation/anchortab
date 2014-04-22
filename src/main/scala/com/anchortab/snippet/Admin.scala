package com.anchortab.snippet

import scala.xml._
import scala.collection.immutable.Range._

import java.text.SimpleDateFormat
import java.util.UUID

import net.liftweb._
  import sitemap._
    import Loc._
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
    import Extraction._
  import mongodb.BsonDSL._

import com.anchortab.model._

import com.stripe

import org.joda.time._

import org.bson.types.ObjectId

object Admin extends AffiliateCalculation with AccountDeletion {
  val usersListMenu = Menu.i("Users") / "admin" / "users"
  val usersNewMenu = Menu.i("New User") / "admin" / "users" / "new" >>
    TemplateBox(() => Templates("admin" :: "user" :: "form" :: Nil))
  val usersEditMenu =
    Menu.param[User]("Edit User", Text("Edit User"), User.find(_), _._id.toString) /
    "admin" / "user" / * >>
    TemplateBox(() => Templates("admin" :: "user" :: "form" :: Nil))
  val plansListMenu = Menu.i("Plans") / "admin" / "plans"
  val plansNewMenu = Menu.i("New Plan") / "admin" / "plans" / "new" >>
    TemplateBox(() => Templates("admin" :: "plan" :: "form" :: Nil))
  val plansEditMenu =
    Menu.param[Plan]("Edit Plan", Text("Edit Plan"), Plan.find(_), _._id.toString) /
    "admin" / "plan" / * >>
    TemplateBox(() => Templates("admin" :: "plan" :: "form" :: Nil))
  val affiliateReportMenu = Menu.i("Affiliate Report") / "admin" / "affiliate-report"

  val menus =
    usersListMenu ::
    usersNewMenu ::
    usersEditMenu ::
    plansListMenu ::
    plansNewMenu ::
    plansEditMenu ::
    affiliateReportMenu ::
    Nil

  val shortDateFormatter = new SimpleDateFormat("MM/dd/yyyy")
  val longDateFormatter = new SimpleDateFormat("MMMM dd, yyyy")

  def snippetHandlers : SnippetPF = {
    case "admin-user-list" :: Nil => adminUserList _
    case "edit-user-form" :: Nil => editUserForm

    case "admin-plans-list" :: Nil => adminPlansList _
    case "edit-plan-form" :: Nil => editPlanForm

    case "affiliate-report" :: Nil => affiliateReport
  }

  def editPlanForm = {
    val requestPlan = plansEditMenu.currentValue

    var planName = requestPlan.map(_.name) openOr ""
    var planDescription = requestPlan.map(_.description) openOr ""
    var planTerm = requestPlan.map(_.term) openOr Plan.MonthlyTerm
    var planPrice = requestPlan.map(_.price) openOr 0.0
    var trialDays = requestPlan.map(_.trialDays) openOr 0
    var visible = requestPlan.map(_.visibleOnRegistration) openOr true
    var special = requestPlan.map(_.isSpecial) openOr false
    var featureBasicAnalytics = requestPlan.map(_.hasFeature_?(Plan.Features.BasicAnalytics)) openOr false
    var featureWhitelabeledTabs = requestPlan.map(_.hasFeature_?(Plan.Features.WhitelabeledTabs)) openOr false
    var featureCustomColorSchemes = requestPlan.map(_.hasFeature_?(Plan.Features.CustomColorSchemes)) openOr false
    var featureApiAccess = requestPlan.map(_.hasFeature_?(Plan.Features.ApiAccess)) openOr false
    var featurePardotIntegration = requestPlan.map(_.hasFeature_?(Plan.Features.PardotIntegration)) openOr false
    var quotaNumberOfTabs = requestPlan.flatMap(_.quotaFor(Plan.Quotas.NumberOfTabs).map(_.toString)) openOr ""
    var quotaEmailSubscriptions = requestPlan.flatMap(_.quotaFor(Plan.Quotas.EmailSubscriptions).map(_.toString)) openOr ""
    var quotaViews = requestPlan.flatMap(_.quotaFor(Plan.Quotas.Views).map(_.toString)) openOr ""

    def submit() = {
      implicit val formats = DefaultFormats ++ JodaTimeSerializers.all

      val features = {
        Map(
          Plan.Features.BasicAnalytics -> featureBasicAnalytics,
          Plan.Features.WhitelabeledTabs -> featureWhitelabeledTabs,
          Plan.Features.CustomColorSchemes -> featureCustomColorSchemes,
          Plan.Features.ApiAccess -> featureApiAccess,
          Plan.Features.PardotIntegration -> featurePardotIntegration
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

      plansEditMenu.currentValue match {
        case Empty =>
          val stripeId: Box[String] = {
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
          }

          stripeId match {
            case fail:Failure =>
              Alert("Error from Stripe: " + fail.toString)

            case Empty =>
              Alert("Something went wrong. Please contact support.")

            case _ =>
              Plan( planName, planDescription, planPrice, trialDays,
                    features, quotas, special, visible, term = planTerm,
                    stripeId = stripeId).save

              RedirectTo("/admin/plans")
          }

        case Full(requestPlan) =>
          val priceChanged = requestPlan.price != planPrice
          val termChanged = requestPlan.term != planTerm
          val trialDaysChanged = requestPlan.trialDays != trialDays
          val nameChanged = requestPlan.name != planName

          val stripePlanIdUpdate: Option[String] = {
            if (priceChanged || termChanged || trialDaysChanged) {
              // Delete old plan, create new.
              requestPlan.stripeId.foreach { stripeId =>
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
            } else if (nameChanged) {
              // Update plan
              requestPlan.stripeId.map { stripeId =>
                tryo(stripe.Plan.retrieve(stripeId)).map { plan =>
                  plan.update(Map("name" -> planName))
                }

                stripeId
              }
            } else {
              // do nothing
              requestPlan.stripeId
            }
          }

          Plan.update("_id" -> requestPlan._id, "$set" -> (
            ("name" -> planName) ~
            ("description" -> planDescription) ~
            ("price" -> planPrice) ~
            ("features" -> features) ~
            ("quotas" -> quotas) ~
            ("isSpecial" -> special) ~
            ("visibleOnRegistration" -> visible) ~
            ("term" -> decompose(planTerm)) ~
            ("trialDays" -> trialDays) ~
            ("stripeId" -> stripePlanIdUpdate)
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
      ".plan-special" #> checkbox(special, special = _) &
      ".feature-basic-analytics" #> checkbox(featureBasicAnalytics, featureBasicAnalytics = _) &
      ".feature-whitelabeled-tabs" #> checkbox(featureWhitelabeledTabs, featureWhitelabeledTabs = _) &
      ".feature-custom-color-schemes" #> checkbox(featureCustomColorSchemes, featureCustomColorSchemes = _) &
      ".feature-api-access" #> checkbox(featureApiAccess, featureApiAccess = _) &
      ".feature-pardot-integration" #> checkbox(featurePardotIntegration, featurePardotIntegration = _) &
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

    def deletePlan(plan: Plan)(s: String) = {
      {
        for {
          stripeId <- plan.stripeId
          stripePlan <- tryo(stripe.Plan.retrieve(stripeId))
          deletedPlan <- tryo(stripePlan.delete)
        } yield {
          plan.delete
          Reload
        }
      } getOrElse {
        Alert("Something went wrong.")
      }
    }

    val planTransform =
      ".plan-row" #> plans.map { plan =>
        ".plan-name *" #> plan.name &
        ".plan-price *" #> plan.price.toString &
        ".plan-term *" #> plan.term.description &
        ".edit-plan [href]" #> plansEditMenu.toLoc.calcHref(plan) &
        ".delete-plan [onclick]" #> onEventIf("Delete this plan?", deletePlan(plan) _)
      }

    planTransform.apply(xhtml)
  }

  def editUserForm = {
    {
      for {
        session <- userSession.is
        currentUser <- User.find(session.userId) if currentUser.admin_?
        user <- usersEditMenu.currentValue or Full(User("", ""))
      } yield {
        var firstName = user.profile.flatMap(_.firstName) getOrElse ""
        var lastName = user.profile.flatMap(_.lastName) getOrElse ""
        var organization = user.profile.flatMap(_.organization) getOrElse ""
        var email = user.email
        var changePassword = ""
        var confirmPassword = ""
        var isAdmin = user.admin_?
        var isAffiliate = user.affiliate_?

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

          (usersEditMenu.currentValue, email, passwordChange) match {
            case (_, "", _) =>
              Alert("Email is a required field. It must have a value.")

            case (_, _, Failure(msg, _, _)) =>
              Alert(msg)

            case (Failure(msg, _, _), _, _) =>
              Alert(msg)

            case (Full(_), _, pw) =>
              User.update("_id" -> user._id, "$set" -> (
                ("email" -> email) ~
                ("profile" -> decompose(userProfile)) ~
                ("password" -> pw.toOption) ~
                ("role" -> (isAdmin ? "admin" | ""))
              ))

              if (isAffiliate && ! user.affiliate_?)
                User.update("_id" -> user._id, "$set" -> ("affiliateCode" -> randomString(32)))
              else if (! isAffiliate && user.affiliate_?)
                User.update("_id" -> user._id, "$unset" -> ("affiliateCode" -> true))

              RedirectTo("/admin/users")

            case (Empty, email, Full(pw)) =>
              val user = User(email, pw, Some(userProfile))

              val userWithRole = {
                if (isAdmin)
                  user.copy(role = Some("admin"))
                else
                  user
              }

              val userWithRoleAndAffiliateCode = {
                if (isAffiliate)
                  user.copy(affiliateCode = Some(randomString(32)))
                else
                  user
              }

              userWithRoleAndAffiliateCode.save

              RedirectTo("/admin/users")

            case (Empty, _, Empty) =>
              Alert("Password is required for new users.")
          }
        }

        val bind =
          ".first-name" #> text(firstName, firstName = _) &
          ".last-name" #> text(lastName, lastName = _) &
          ".organization" #> text(organization, organization = _) &
          ".email" #> text(email, email = _) &
          ".change-password" #> password(changePassword, changePassword = _) &
          ".confirm-password" #> password(confirmPassword, confirmPassword = _) &
          ".admin-checkbox" #> checkbox(isAdmin, isAdmin = _) &
          ".affiliate-checkbox" #> checkbox(isAffiliate, isAffiliate = _) &
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
      RedirectTo("/admin/user/" + userId.toString)
    }

    def impersonateUser(userId: ObjectId)(s: String) = {
      Authentication.impersonateUser(userId)
    }

    def deleteUser(user: User)(s: String) = {
      deleteAccount(user) match {
        case Full(true) =>
          Notices.notice(s"Account for ${user.email} deleted.")
          Reload

        case somethingElse =>
          Alert(somethingElse.toString)
      }
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
            ".edit-user [onclick]" #> onEvent(editUser(user._id) _) &
            ".delete-user [onclick]" #> onEventIf("This will erase the user and all their tabs. May take a few moments. Are you sure?", deleteUser(user) _)
          }
        }
      }

    userListTransform.map(_.apply(xhtml)) openOr NodeSeq.Empty
  }

  def renderAffiliateReport(targetMonth: YearMonth, plans: List[Plan]) = {
    val affiliates = User.findAll("affiliateCode" -> ("$exists" -> true)).map { user =>
      val allActiveSubs = totalActiveReferralsAsOfTargetMonth(user._id, targetMonth)
      val newActiveSubs = newReferralsForTargetMonth(user._id, targetMonth)

      (user, allActiveSubs.totalByPlan, newActiveSubs.totalByPlan)
    }

    ".affiliate-report-row" #> affiliates.map {
      case (affiliate, allActiveSubs, newActiveSubs) =>
        ".affiliate-email *" #> affiliate.email &
        ".affiliate-name *" #> affiliate.name &
        ".new-subscription-summary-item" #> plans.map { plan =>
          ".plan-name *" #> plan.name &
          ".subscribe-count *" #> (newActiveSubs.get(plan.name).getOrElse(0))
        } &
        ".total-subscription-summary-item" #> plans.map { plan =>
          ".plan-name *" #> plan.name &
          ".subscribe-count *" #> (allActiveSubs.get(plan.name).getOrElse(0))
        }
    }
  }

  def affiliateReport = {
    var targetMonth: YearMonth = new YearMonth(DateTime.now.getYear(), DateTime.now.getMonthOfYear())

    val months = Seq(
      (1, "January"),
      (2, "February"),
      (3, "March"),
      (4, "April"),
      (5, "May"),
      (6, "June"),
      (7, "July"),
      (8, "August"),
      (9, "September"),
      (10, "October"),
      (11, "November"),
      (12, "December")
    )

    val currentYear = (new DateTime()).getYear()
    val years = (new Inclusive(2013, currentYear, 1)).toList.map(year => (year, year.toString))
    val plans = Plan.findAll(JObject(Nil))

    "#affiliate-report-container" #> idMemoize { renderer =>
      ClearClearable andThen
      "#month-selection" #> SHtml.ajaxSelectObj[Int](
        months,
        Full(targetMonth.getMonthOfYear()),
        { month: Int =>
          targetMonth = targetMonth.withMonthOfYear(month)
          Noop
        }
      ) &
      "#year-selection" #> SHtml.ajaxSelectObj[Int](
        years,
        Full(targetMonth.getYear()),
        { year: Int =>
          targetMonth = targetMonth.withYear(year)
          Noop
        }
      ) &
      "#update-report [onclick]" #> ajaxInvoke(() => renderer.setHtml) andThen
      renderAffiliateReport(targetMonth, plans)
    }
  }
}
