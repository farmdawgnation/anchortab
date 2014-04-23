package com.anchortab.snippet

import scala.xml._

import net.liftweb._
  import common._
  import markdown._
  import http._
    import js._
      import JsCmds._
    import SHtml._
  import sitemap._
    import Loc._
  import util._
    import Helpers._

import com.anchortab.model._
import com.anchortab.actor._

object EmailUsers {
  val menu = Menu.i("Email Users") / "admin" / "email-users" >>
    If(
      () => userSession.is.map(_.userId).flatMap(id => User.find(id)).map(_.admin_?).getOrElse(false),
      () => NotFoundResponse()
    )
}

class EmailUsers {
  var messageType: AnchorTabEmailType = AnnouncementEmail
  var subject = ""
  var message = ""

  def sendEmail() = {
    val renderedMessage = Unparsed(new ActuariusTransformer()(message))
    EmailActor ! SendMassEmail(messageType, subject, renderedMessage)
    Notices.notice("Email sent.")
    Reload
  }

  def render = {
    makeFormsAjax andThen
    "#message-type" #> selectObj[AnchorTabEmailType](
      (AnnouncementEmail :: AlertEmail :: UrgentNotice :: Nil).map(theType => (theType, theType.toString)),
      Full(messageType),
      selectedType => messageType = selectedType
    ) &
    "#subject" #> text(subject, subject = _) &
    "#message" #> textarea(message, message = _) &
    "#send-email" #> ajaxOnSubmit(sendEmail _)
  }
}
