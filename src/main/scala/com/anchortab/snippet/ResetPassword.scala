package com.anchortab.snippet

import scala.xml._

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
  import mongodb.BsonDSL._

import com.anchortab.model.{User, UserPasswordResetKey}

object ResetPassword {
  /**
   * Sitemap menu helpers.
  **/
  protected def userForReset(passwordResetKey: String): Box[User] = {
    for {
      user <- User.findAll("passwordResetKey.key" -> passwordResetKey).headOption
        if user.passwordResetKey.map(_.expires isAfterNow).getOrElse(false)
    } yield {
      user
    }
  }

  protected def resetForUser(user: User) = {
    user.passwordResetKey.filter(_.expires isAfterNow).map(_.key) getOrElse ""
  }

  val menu =
    Menu.param[User]("Reset Password", Text("Reset Password"), userForReset(_), resetForUser(_)) /
    "lost-sticky-note" / * >>
    TemplateBox(() => Templates("lost-sticky-note" :: Nil))
}

class ResetPassword(user: User) extends Loggable {
  private var newPassword = ""
  private var confirmPassword = ""

  private def processResetPassword = {
    if (newPassword.trim.length == 0 || confirmPassword.trim.length == 0) {
      FormValidationError(".password", "") &
      FormValidationError(".password-confirmation", "Please choose a password with characters.")
    } else if(newPassword != confirmPassword) {
      FormValidationError(".password", "") &
      FormValidationError(".password-confirmation" ,"Your passwords do not match.")
    } else {
      user.copy(
        password = User.hashPassword(newPassword),
        passwordResetKey = None
      ).save

      RedirectTo(Authentication.managerMenu.loc.calcDefaultHref)
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    ".password" #> password(newPassword, newPassword = _) &
    ".password-confirmation" #> password(confirmPassword, confirmPassword = _) &
    ".submit" #> ajaxSubmit("Reset Password", processResetPassword _)
  }
}
