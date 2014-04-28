package com.anchortab.snippet

import net.liftweb._
  import common._
  import sitemap._
    import Loc._
  import http._
    import SHtml._
  import util._
    import Helpers._
  import js._
    import JsCmds._

import com.anchortab.model.{User, UserPasswordResetKey}
import com.anchortab.actor._

object ForgotPassword {
  val menu = Menu.i("Forgot Password") / "amnesia" >>
    Authentication.ifNotLoggedIn
}

class ForgotPassword extends Loggable {
  private var recoveryEmailAddress = ""

  private def processForgotPassword = {
    {
      for {
        user <- User.forEmail(recoveryEmailAddress)
        request <- S.request
      } yield {
        val resetKey = UserPasswordResetKey()
        val userWithReset = user.copy(passwordResetKey = Some(resetKey))
        userWithReset.save

        val resetLink = "http://" + request.hostName + ResetPassword.menu.toLoc.calcHref(userWithReset)

        EmailActor ! SendForgotPasswordEmail(userWithReset.email, resetLink)

        RedirectTo(ForgotPasswordComplete.menu.loc.calcDefaultHref)
      }
    } getOrElse {
      FormValidationError(".email-address", "We don't have an account with that email.")
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".email-address" #> text(recoveryEmailAddress, recoveryEmailAddress = _) &
    ".submit" #> ajaxSubmit("Send Password Reset", processForgotPassword _)
  }
}
