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

import com.anchortab.model._

object userSession extends SessionVar[Box[UserSession]](Empty)
object statelessUser extends RequestVar[Box[User]](Empty)

object Authentication extends Loggable {
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
    // FIXME
    logger.info("Logged in with credentials: " + username + " " + password)
  }

  def registrationForm = {
    var requestedUsername = ""
    var requestedPassword = ""
    var firstName = ""
    var lastName = ""
    var organization = ""
    var emailAddress = ""

    def processRegistration = {
      // FIXME
    }

    val bind =
      ".username" #> text(requestedUsername, requestedUsername = _) &
      ".password" #> password(requestedPassword, requestedPassword = _) &
      ".first-name" #> text(firstName, firstName = _) &
      ".last-name" #> text(lastName, lastName = _) &
      ".organization" #> text(organization, organization = _) &
      ".email" #> text(emailAddress, emailAddress = _) &
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
