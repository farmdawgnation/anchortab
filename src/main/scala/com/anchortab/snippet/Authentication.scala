package com.anchortab.snippet

import scala.xml._

import net.liftweb._
  import common._
  import sitemap._
    import Loc._
  import http._
    import SHtml._
    import S.SFuncHolder
    import provider._
    import js._
      import JsCmds._
    import LiftRules._
  import util._
    import Helpers._
  import json._
  import mongodb.BsonDSL._

import com.mongodb.WriteConcern

import org.bson.types.ObjectId

import com.anchortab.model._
import com.anchortab.actor._

import com.stripe

case object LoginFailed extends SimpleAnchorTabEvent("login-failed")
case class RedirectingToWePay(preapprovalUrl: String) extends SimpleAnchorTabEvent("redirecting-to-wepay")
case class FormValidationError(fieldSelector: String, error: String) extends
  SimpleAnchorTabEvent("form-validation-error")

object userSession extends SessionVar[Box[UserSession]](Empty)
object impersonatorSession extends SessionVar[Box[UserSession]](Empty)
object statelessUser extends RequestVar[Box[User]](Empty)
object passwordResetUser extends RequestVar[Box[User]](Empty)

object Authentication extends Loggable {
  /**
   * Sitemap menus.
  **/
  val managerMenu = Menu.i("Manager") / "manager"

  val menus =
    managerMenu ::
    Nil

  /**
   * This method sets various sticky notices when a user logs in if their
   * account is in such a state that it requires those notices.
  **/
  def authenticationStickyNotices(user: User) = {
    Notices.removeAllStickyNotices

    // Set sticky notice for quota error if needed.
    if (user.subscription.isDefined && ! user.tabsActive_?)
      Notices.warning("Your tabs are inactive. You may upgrade your plan to reactivate your tabs.", Some("tab-shutdown-error"))

    // Set IE 8 sticky notice
    if (S.isIE8)
      Notices.warning("Anchor Tab does not support IE 8. Your tabs won't display for you in IE 8 and this site may not work correctly. Please upgrade or switch to Firefox or Chrome.", Some("ie-8-warning"))
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

  def earlyInStateful(req:Box[Req]) = {
    (userSession.is, S.findCookie("session")) match {
      case (Empty, Full(cookie:HTTPCookie)) =>
        for {
          cookieValue <- cookie.value
          sessionId <- tryo(new ObjectId(cookieValue))
          dbSession <- UserSession.find(sessionId)
          user <- dbSession.user
        } {
          // Set any sticky notices that are needed.
          authenticationStickyNotices(user)

          // Record session.
          val remoteIp = S.containerRequest.map(_.remoteAddress).openOr("localhost")
          val userAgent = S.containerRequest.flatMap(_.userAgent).openOr("unknown")

          val session = UserSession(dbSession.userId, remoteIp, userAgent)
          session.save
          userSession(Full(session))
        }

      case _ =>
    }
  }

  def dispatch : DispatchPF = {
    case Req("session" :: "logout" :: Nil, _, _) =>
      () => {
        if (impersonatorSession.is.isDefined) {
          userSession(impersonatorSession.is)
          impersonatorSession(Empty)
          Full(RedirectResponse(Admin.usersListMenu.loc.calcDefaultHref))
        } else {
          userSession(Empty)
          S.session.foreach(_.destroySession)
          Full(RedirectResponse("/", HTTPCookie("session", "deleted").setPath("/").setMaxAge(-100)))
        }
      }

    case Req("session" :: "login" :: Nil, _, _) =>
      () => {
        for {
          session <- userSession.is
        } yield {
          RedirectResponse(Dashboard.dashboardMenu.loc.calcDefaultHref, HTTPCookie("session", session._id.toString).setPath("/"))
        }
      }
  }

  def snippetHandlers : SnippetPF = {
    case "login-form" :: Nil => loginForm
    case "redirect-to-dashboard-if-logged-in" :: Nil => redirectToDashboardIfLoggedIn
    case "pwn-if-not-logged-in" :: Nil => pwnIfNotLoggedIn
    case "show-if-logged-in" :: Nil => showIfLoggedIn
    case "pwn-if-not-admin" :: Nil => pwnIfNotAdmin
    case "show-if-admin" :: Nil => showIfAdmin
    case "show-if-affiliate" :: Nil => showIfAffiliate

    case "register-influencer" :: Nil => registerButton("The Influencer")
  }

  val planIdMap = scala.collection.concurrent.TrieMap[String, String]()
  def registerButton(planName: String) = {
    val planId = planIdMap.getOrElseUpdate(planName, Plan.find("name" -> planName).map(_._id.toString).getOrElse(""))

    "button [data-plan-id]" #> planId
  }

  def redirectToDashboardIfLoggedIn(ns:NodeSeq) = {
    if (userSession.isDefined)
      S.redirectTo(Dashboard.dashboardMenu.loc.calcDefaultHref)

    ns
  }

  def pwnIfNotLoggedIn(ns:NodeSeq) = {
    if (! userSession.isDefined)
      S.redirectTo(managerMenu.loc.calcDefaultHref)

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
      // Let's be clever here and trigger a 404 so that someone doing random
      // probes on our app will believe there simply is no /admin url.
      throw new ResponseShortcutException(NotFoundResponse())
    }
  }

  def showIfAffiliate(ns: NodeSeq) = {
    {
      for {
        session <- userSession
        user <- User.find(session.userId) if user.affiliate_?
      } yield {
        ns
      }
    } openOr {
      NodeSeq.Empty
    }
  }

  def showIfAdmin(ns:NodeSeq) = {
    {
      for {
        session <- userSession
        user <- User.find(session.userId) if user.admin_?
      } yield {
        ns
      }
    } openOr {
      NodeSeq.Empty
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

    if (userSession.is.isDefined) {
      "form" #> <p><button class="go-to-manager">Manager &raquo;</button></p>
    } else {
      val bind =
        ".email" #> text(submittedUsername, submittedUsername = _) &
        ".password" #> password(submittedPassword, submittedPassword = _) &
        ".submit" #> ajaxSubmit("Log In", () => processLogin(submittedUsername, submittedPassword))

      "form" #> { ns:NodeSeq =>
        ajaxForm(bind(ns))
      }
    }
  }

  // This actually does need to be a first-class citizen because we use it in
  // multiple places. :(
  def processLogin(username:String, password:String) = {
    User.attemptLogin(username, password) match {
      case Full(user) =>
        val remoteIp = S.containerRequest.map(_.remoteAddress).openOr("localhost")
        val userAgent = S.containerRequest.flatMap(_.userAgent).openOr("unknown")

        val session = UserSession(user._id, remoteIp, userAgent)
        session.save
        userSession(Full(session))

        authenticationStickyNotices(user)

        RedirectTo("/session/login")

      case _ =>
        LoginFailed
    }
  }

  private[snippet] def impersonateUser(userId: ObjectId) = {
    User.find(userId) match {
      case Some(user) =>
        impersonatorSession(userSession.is)

        val remoteIp = S.containerRequest.map(_.remoteAddress).openOr("localhost")
        val userAgent = S.containerRequest.flatMap(_.userAgent).openOr("unknown")

        // We don't persist this like a normal user session, because it doesn't need to
        // outlive the Lift session.
        userSession(Full(UserSession(user._id, remoteIp, userAgent)))

        RedirectTo(Dashboard.dashboardMenu.loc.calcDefaultHref)

      case _ =>
        Alert("Something went wrong with impersonation.")
    }
  }
}
