package com.anchortab.snippet

import scala.xml.NodeSeq

import net.liftweb._
  import common._
  import http._
    import provider._
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

import org.bson.types.ObjectId

import com.anchortab.model._
import com.anchortab.actor._

import me.frmr.wepay._
  import api.Preapproval

case object LoginFailed extends SimpleAnchorTabEvent("login-failed")
case class RedirectingToWePay(preapprovalUrl: String) extends SimpleAnchorTabEvent("redirecting-to-wepay")
case class FormValidationError(fieldSelector: String, error: String) extends
  SimpleAnchorTabEvent("form-validation-error")

object userSession extends SessionVar[Box[UserSession]](Empty)
object impersonatorSession extends SessionVar[Box[UserSession]](Empty)
object statelessUser extends RequestVar[Box[User]](Empty)
object passwordResetUser extends RequestVar[Box[User]](Empty)

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

  def earlyInStateful(req:Box[Req]) = {
    (userSession.is, S.findCookie("session")) match {
      case (Empty, Full(cookie:HTTPCookie)) =>
        for {
          cookieValue <- cookie.value
          sessionId <- tryo(new ObjectId(cookieValue))
          dbSession <- UserSession.find(sessionId)
        } {
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
          Full(RedirectResponse("/admin/users"))
        } else {
          userSession(Empty)
          Full(RedirectResponse("/", HTTPCookie("session", "deleted").setPath("/").setMaxAge(-100)))
        }
      }

    case Req("session" :: "login" :: Nil, _, _) =>
      () => {
        for {
          session <- userSession.is
        } yield {
          RedirectResponse("/manager/dashboard", HTTPCookie("session", session._id.toString).setPath("/"))
        }
      }
  }

  def statelessRewrite : RewritePF = {
    case RewriteRequest(ParsePath("lost-sticky-note" :: passwordResetKey :: Nil, _, _, _), _, _) =>
      {
        for {
          user <- User.findAll("passwordResetKey.key" -> passwordResetKey).headOption
            if user.passwordResetKey.map(_.expires isAfterNow).getOrElse(false)
        } yield {
          passwordResetUser(Full(user))
          RewriteResponse("lost-sticky-note" :: Nil)
        }
      } getOrElse {
        S.redirectTo("/amnesia")
      }
  }

  def snippetHandlers : SnippetPF = {
    case "login-form" :: Nil => loginForm
    case "registration-form" :: Nil => registrationForm
    case "forgot-password-form" :: Nil => forgotPasswordForm
    case "reset-password-form" :: Nil => resetPasswordForm
    case "redirect-to-dashboard-if-logged-in" :: Nil => redirectToDashboardIfLoggedIn
    case "pwn-if-not-logged-in" :: Nil => pwnIfNotLoggedIn
    case "show-if-logged-in" :: Nil => showIfLoggedIn
    case "pwn-if-not-admin" :: Nil => pwnIfNotAdmin
    case "show-if-admin" :: Nil => showIfAdmin
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
      // Let's be clever here and trigger a 404 so that someone doing random
      // probes on our app will believe there simply is no /admin url.
      throw new ResponseShortcutException(NotFoundResponse())
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
  private def processLogin(username:String, password:String) = {
    User.attemptLogin(username, password) match {
      case Full(user) =>
        val remoteIp = S.containerRequest.map(_.remoteAddress).openOr("localhost")
        val userAgent = S.containerRequest.flatMap(_.userAgent).openOr("unknown")

        val session = UserSession(user._id, remoteIp, userAgent)
        session.save
        userSession(Full(session))

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

        RedirectTo("/manager/dashboard")

      case _ =>
        Alert("Something went wrong with impersonation.")
    }
  }

  def registrationForm = {
    // We only allow registration with invite code for the time being.
    if (Props.mode == Props.RunModes.Production && ! inviteCode.is.isDefined)
      S.redirectTo("/")

    var emailAddress = ""
    var requestedPassword = ""
    var requestedPasswordConfirmation = ""
    var firstName = ""
    var lastName = ""
    var organization = ""
    var selectedPlan = ""

    val plans = {
      inviteCode.is.flatMap(_.forPlan).map(List(_))
    } openOr {
      Plan.findAll("visibleOnRegistration" -> true)
    }

    val planSelections = plans.map { plan =>
      (plan._id.toString, plan.registrationTitle)
    }

    def generateSubscriptionForPlan(plan:Plan) = {
      if (plan.free_?) {
        Full(UserSubscription(plan._id, plan.price, plan.term, status="active"))
      } else {
        for {
          accountId <- Props.getLong("wepay.anchorTabAccountId") ?~! "No WePay Account ID found."
          redirectUri <- Props.get("wepay.anchorTabRedirectUri") ?~! "No redirect URI found."
          callbackUri <- Props.get("wepay.anchorTabCallbackUri") ?~! "No callback URI found."
          preapproval <- Preapproval(accountId, plan.price, plan.name, plan.term.description,
                                    redirect_uri = Some(redirectUri), callback_uri = Some(callbackUri),
                                    fee_payer = Some("payee"), auto_recur = Some(true)).save
          preapprovalId = preapproval.preapproval_id
          preapprovalUri <- preapproval.preapproval_uri
        } yield {
          UserSubscription(plan._id, plan.price, plan.term, Some(preapprovalId), Some(preapprovalUri))
        }
      }
    }

    def processRegistration = {
      val validators = Map(
        "input.email-address" -> (() =>
          if (emailAddress.nonEmpty)
            if (User.count("email" -> emailAddress) > 0)
              Full("That email address is already in use by another user.")
            else
              Empty
          else
            Full("Email address is required.")
        ),
        "input.password" -> (() =>
          if (requestedPassword.nonEmpty)
            Empty
          else
            Full("Password is required.")
        ),
        "input.password-confirmation" -> (() =>
          if (requestedPasswordConfirmation.nonEmpty)
            if (requestedPassword != requestedPasswordConfirmation)
              Full("Password and Confirm Password must match.")
            else
              Empty
          else
            Full("Password confirmation is required.")
        )
      )

      val validationErrors: List[FormValidationError] = validators.flatMap { validator =>
        val validatorSelector = validator._1
        val validatorFunc = validator._2

        validatorFunc() match {
          case Full(validationError) => Some(FormValidationError(validatorSelector, validationError))
          case _ => None
        }
      }.toList

      validationErrors match {
        case Nil =>
          val user : Box[User] =
            for {
              plan <- (Plan.find(selectedPlan):Box[Plan]) ?~! "Plan could not be located."
              subscription <- generateSubscriptionForPlan(plan)
            } yield {
              val firstSteps = Map(
                UserFirstStep.Keys.ConnectAnExternalService -> UserFirstStep.Steps.ConnectAnExternalService,
                UserFirstStep.Keys.CreateATab -> UserFirstStep.Steps.CreateATab,
                UserFirstStep.Keys.EmbedYourTab -> UserFirstStep.Steps.EmbedYourTab
              )

              User(emailAddress, User.hashPassword(requestedPassword),
                   Some(UserProfile(Some(firstName), Some(lastName), Some(organization))),
                   subscriptions = List(subscription), firstSteps = firstSteps)
            }

          user.foreach(_.save)

          user match {
            case Full(user) =>
              val loginResult = processLogin(emailAddress, requestedPassword)

              // Bump the invite code count
              for {
                invite <- inviteCode.is
              } {
                InviteCode.update("_id" -> invite._id, "$inc" -> ("numberOfUses" -> 1))
              }

              // Send welcome email
              EmailActor ! SendWelcomeEmail(user.email)

              if (plans.filter(_._id.toString == selectedPlan).headOption.map(_.free_?) getOrElse true) {
                loginResult
              } else {
                val wepayUri =
                  user.subscriptions.headOption.flatMap(_.preapprovalUri) getOrElse "#"

                // Alert the user they're about to be redirected to WePay for payment, and
                // then do the redirection.
                RedirectingToWePay(wepayUri)
              }

            case m =>
              logger.error("While registering account got: " + m)
              Alert("Something went wrong during account creation.")
          }

        case errors =>
          errors.foldLeft(Noop)(_ & _)
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

    def processForgotPassword = {
      {
        for {
          user <- User.findAll("email" -> recoveryEmailAddress).headOption
          request <- S.request
        } yield {
          val resetKey = UserPasswordResetKey()
          val userWithReset = user.copy(passwordResetKey = Some(resetKey))
          userWithReset.save

          val resetLink = "http://" + request.hostName + "/lost-sticky-note/" + resetKey.key

          EmailActor ! SendForgotPasswordEmail(userWithReset.email, resetLink)

          RedirectTo("/amnesia-complete")
        }
      } getOrElse {
        FormValidationError(".email-address", "We don't have an account with that email.")
      }
    }

    val bind =
      ".email-address" #> text(recoveryEmailAddress, recoveryEmailAddress = _) &
      ".submit" #> ajaxSubmit("Send Password Reset", processForgotPassword _)

    "form" #> { ns:NodeSeq =>
      ajaxForm(bind(ns))
    }
  }

  def resetPasswordForm = {
    var newPassword = ""
    var confirmPassword = ""

    def processResetPassword(user: User)() = {
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

        RedirectTo("/manager")
      }
    }

    {
      for {
        user <- passwordResetUser
      } yield {
        val bind =
          ".password" #> password(newPassword, newPassword = _) &
          ".password-confirmation" #> password(confirmPassword, confirmPassword = _) &
          ".submit" #> ajaxSubmit("Reset Password", processResetPassword(user) _)

        "form" #> { ns:NodeSeq =>
          ajaxForm(bind(ns))
        }
      }
    } openOr {
      S.redirectTo("/amnesia")
    }
  }
}
