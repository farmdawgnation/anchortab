package com.anchortab.snippet

import scala.xml.NodeSeq

import net.liftweb._
  import common._
  import http._
    import SHtml._
    import LiftRules._
  import util._
    import Helpers._


object Authentication extends Loggable {
  def snippetHandlers : SnippetPF = {
    case "login-form" :: Nil => loginForm
    case "registration-form" :: Nil => registrationForm
    case "forgot-password-form" :: Nil => forgotPasswordForm
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
