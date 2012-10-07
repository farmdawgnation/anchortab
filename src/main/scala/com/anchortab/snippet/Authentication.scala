package com.anchortab.snippet

import scala.xml.NodeSeq

import net.liftweb._
  import common._
  import http._
    import js._
      import JsCmds._
    import SHtml._
    import LiftRules._
  import util._
    import Helpers._
  import json._
    import JsonDSL._
  import mongodb.BsonDSL._

import com.mongodb.WriteConcern

import com.anchortab.model._

import me.frmr.wepay._
  import api.Preapproval

object userSession extends SessionVar[Box[UserSession]](Empty)
object statelessUser extends RequestVar[Box[User]](Empty)

object Authentication extends Loggable {
  implicit val authenticationToken : Option[WePayToken] = {
    for {
      anchortabUserId <- Props.getLong("wepay.anchorTabUserId")
      anchortabAccessToken <- Props.get("wepay.anchorTabAccessToken")
    } yield {
      WePayToken(anchortabUserId, anchortabAccessToken, "BEARER", None)
    }
  }

  /**
   * Handle authentication for stateless requests (the API).
  **/
  def earlyInStateless(req:Box[Req]) = {
    req match {
      case Full(req:Req) if req.header("Authorization").isDefined =>
        req.header("Authorization").foreach { authHeader =>
          val authParts = authHeader.split(" ")

          for {
            authType <- tryo(authParts(0)) if authType == "Bearer"
            authKey <- tryo(authParts(1))
            user <- User.find("authorizations.key" -> authKey)
          } {
            statelessUser(Full(user))
          }
        }

      case _ =>
        // Nada
    }
  }

  def snippetHandlers : SnippetPF = {
    case "login-form" :: Nil => loginForm
    case "registration-form" :: Nil => registrationForm
    case "forgot-password-form" :: Nil => forgotPasswordForm
    case "redirect-to-dashboard-if-logged-in" :: Nil => redirectToDashboardIfLoggedIn
    case "pwn-if-not-logged-in" :: Nil => pwnIfNotLoggedIn
    case "show-if-logged-in" :: Nil => showIfLoggedIn
    case "pwn-if-not-admin" :: Nil => pwnIfNotAdmin
  }

  def redirectToDashboardIfLoggedIn(ns:NodeSeq) = {
    if (userSession.isDefined)
      S.redirectTo("/manager/dashboard")

    ns
  }

  def pwnIfNotLoggedIn(ns:NodeSeq) = {
    if (! userSession.isDefined)
      S.redirectTo("/manager")

    ns
  }

  def pwnIfNotAdmin(ns:NodeSeq) = {
    {
      for {
        session <- userSession
        user <- User.find(session.userId) if user.admin_?
      } yield {
        ns
      }
    } openOr {
      S.redirectTo("/manager")
    }
  }

  def showIfLoggedIn(ns:NodeSeq) = {
    if (userSession.isDefined)
      ns
    else
      NodeSeq.Empty
  }

  def loginForm = {
    var submittedUsername = ""
    var submittedPassword = ""

    val bind =
      ".username" #> text(submittedUsername, submittedUsername = _) &
      ".password" #> password(submittedPassword, submittedPassword = _) &
      ".submit" #> ajaxSubmit("Log In", () => processLogin(submittedUsername, submittedPassword))

    "form" #> { ns:NodeSeq =>
      ajaxForm(bind(ns))
    }
  }

  private def processLogin(username:String, password:String) = {
    User.attemptLogin(username, password) match {
      case Full(user) =>
        val remoteIp = S.containerRequest.map(_.remoteAddress).openOr("localhost")
        val userAgent = S.containerRequest.flatMap(_.userAgent).openOr("unknown")

        val session = UserSession(user._id, remoteIp, userAgent)
        session.save
        userSession(Full(session))

        RedirectTo("/manager/dashboard")

      case _ =>
        Alert("That username or password appears to be invalid.")
    }
  }

  def registrationForm = {
    var emailAddress = ""
    var requestedPassword = ""
    var requestedPasswordConfirmation = ""
    var firstName = ""
    var lastName = ""
    var organization = ""
    var selectedPlan = ""

    val plans = Plan.findAll("visibleOnRegistration" -> true)
    val planSelections = plans.map { plan =>
      (plan._id.toString, plan.registrationTitle)
    }

    def generateSubscriptionForPlan(plan:Plan) = {
      if (plan.free_?) {
        Full(UserSubscription(plan._id, plan.price, plan.term, status="active"))
      } else {
        for {
          accountId <- Props.getLong("wepay.anchorTabAccountId") ?~! "No WePay Account ID found."
          preapproval <- Preapproval(accountId, plan.price, plan.name, plan.term.description,
                                    fee_payer = Some("payee"), auto_recur = Some(true)).save
          preapprovalId = preapproval.preapproval_id
          preapprovalUri <- preapproval.preapproval_uri
        } yield {
          UserSubscription(plan._id, plan.price, plan.term, Some(preapprovalId), Some(preapprovalUri))
        }
      }
    }

    def processRegistration = {
      // This may be poor design, but it's better than nesting right?
      emailAddress match {
        case "" =>
          Alert("Email address is required.")

        case _ if requestedPassword == "" || requestedPasswordConfirmation == "" =>
          Alert("Both password fields are required.")

        case _ if User.count("email" -> emailAddress) > 0 =>
          Alert("That email address is already in use by another user.")

        case _ if requestedPassword != requestedPasswordConfirmation =>
          Alert("The password and confirm password fields do not match.")

        case _ =>
          val user : Box[User] =
            for {
              plan <- (Plan.find(selectedPlan):Box[Plan]) ?~! "Plan could not be located."
              subscription <- generateSubscriptionForPlan(plan)
            } yield {
              User(emailAddress, User.hashPassword(requestedPassword),
                   Some(UserProfile(Some(firstName), Some(lastName), Some(organization))),
                   subscriptions = List(subscription))
            }

          user.foreach(_.save)

          user match {
            case Full(user) =>
              val loginResult = processLogin(emailAddress, requestedPassword)

              if (plans.filter(_._id.toString == selectedPlan).headOption.map(_.free_?) getOrElse true) {
                loginResult
              } else {
                val wepayUri =
                  user.subscriptions.headOption.flatMap(_.preapprovalUri) getOrElse "#"

                // Alert the user they're about to be redirected to WePay for payment, and
                // then do the redirection.
                Alert("You will now be sent to WePay to complete payment.") &
                RedirectTo(wepayUri)
              }

            case m =>
              logger.error("While registering account got: " + m)
              Alert("Something went wrong during account creation.")
          }
      }
    }

    val bind =
      ".plan-selection" #> select(planSelections, Empty, selectedPlan = _) &
      ".email-address" #> text(emailAddress, emailAddress = _) &
      ".password" #> password(requestedPassword, requestedPassword = _) &
      ".password-confirmation" #> password(requestedPasswordConfirmation, requestedPasswordConfirmation = _) &
      ".first-name" #> text(firstName, firstName = _) &
      ".last-name" #> text(lastName, lastName = _) &
      ".organization" #> text(organization, organization = _) &
      ".submit" #> ajaxSubmit("Register", () => processRegistration)

    "form" #> { ns:NodeSeq =>
      ajaxForm(bind(ns))
    }
  }

  def forgotPasswordForm = {
    var recoveryEmailAddress = ""

    val bind =
      ".recovery-email" #> text(recoveryEmailAddress, recoveryEmailAddress = _) &
      ".submit" #> ajaxSubmit("Send Reset Email", () => processForgotPassword(recoveryEmailAddress))

    "form" #> { ns:NodeSeq =>
      ajaxForm(bind(ns))
    }
  }

  private def processForgotPassword(email:String) = {
    // FIXME
    logger.info("Password recovery requested for " + email)
  }
}
