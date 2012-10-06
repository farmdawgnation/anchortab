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
          var preapprovalUri : Box[String] = Empty

          val user : Box[User] =
            for {
              plan <- (Plan.find(selectedPlan):Box[Plan]) ?~! "Plan could not be located."
              accountId <- Props.getLong("wepay.anchorTabAccountId")
            } yield {
              val subscription = {
                if (plan.free_?) {
                  List(UserSubscription(plan._id, plan.price, 0, plan.term, status = "active"))
                } else {
                  val subscriptionPreapproval = Preapproval(accountId, plan.price, plan.name, plan.term.description,
                                                            fee_payer = Some("payee"), auto_recur = Some(true)).save

                  val preapprovalId = subscriptionPreapproval.map(_.preapproval_id) openOr 0l
                  preapprovalUri = subscriptionPreapproval.flatMap(_.preapproval_uri)

                  if (preapprovalId > 0l) {
                    List(UserSubscription(plan._id, plan.price, preapprovalId, plan.term))
                  } else {
                    logger.error("Fail to init Preapproval: " + subscriptionPreapproval)
                    List()
                  }
                }
              }

              User(emailAddress, User.hashPassword(requestedPassword),
                   Some(UserProfile(Some(firstName), Some(lastName), Some(organization))),
                   subscriptions = subscription)
            }

          user.foreach(_.save)

          user match {
            case Full(user) if preapprovalUri.isDefined =>
              // Do login
              processLogin(emailAddress, requestedPassword)

              // Redirect to WePay
              RedirectTo(preapprovalUri openOr "#")

            case Full(user) =>
              processLogin(emailAddress, requestedPassword)

            case m =>
              logger.error("While registering account got: " + m)
              Alert("Something went wrong during account creation.")
          }
      }
    }

    val plans = Plan.findAll("visibleOnRegistration" -> true).map { plan =>
      (plan._id.toString, plan.registrationTitle)
    }

    val bind =
      ".plan-selection" #> select(plans, Empty, selectedPlan = _) &
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
