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

  def processLogin(username:String, password:String) = {
    // FIXME
    logger.info("Logged in with credentials: " + username + " " + password)
  }

  def registrationForm = {
    PassThru
  }

  def forgotPasswordForm = {
    PassThru
  }
}
