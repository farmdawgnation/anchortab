package com.anchortab.snippet

import scala.xml._

import net.liftweb._
  import common._
  import http._
    import S.SFuncHolder
    import provider._
    import js._
      import JsCmds._
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

import com.stripe

object AuthenticationSHtml extends SHtml {
  private def selected(in: Boolean) = if (in) new UnprefixedAttribute("selected", "selected", Null) else Null

  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission
   *
   * @param opts -- the options.  A list of value and text pairs
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def selectPlans(opts: Seq[(String, String, String)], deflt: Box[String],
               strFunc: (String)=>Any, attrs: ElemAttr*): Elem = {
    val func = S.SFuncHolder(strFunc)
    val vals = opts.map(_._2)
    val testFunc = S.LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)

    attrs.foldLeft(S.fmapFunc(testFunc)(fn => <select name={fn}>{opts.flatMap {case (hasTrial, value, text) => (<option data-has-trial={hasTrial} value={value}>{text}</option>) % selected(deflt.exists(_ == value))}}</select>))(_ % _)
  }
}

case object LoginFailed extends SimpleAnchorTabEvent("login-failed")
case class RedirectingToWePay(preapprovalUrl: String) extends SimpleAnchorTabEvent("redirecting-to-wepay")
case class FormValidationError(fieldSelector: String, error: String) extends
  SimpleAnchorTabEvent("form-validation-error")

object userSession extends SessionVar[Box[UserSession]](Empty)
object impersonatorSession extends SessionVar[Box[UserSession]](Empty)
object statelessUser extends RequestVar[Box[User]](Empty)
object passwordResetUser extends RequestVar[Box[User]](Empty)

object Authentication extends Loggable {
  import AuthenticationSHtml._

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
      for {
        user <- User.findAll("passwordResetKey.key" -> passwordResetKey).headOption
          if user.passwordResetKey.map(_.expires isAfterNow).getOrElse(false)
      } yield {
        passwordResetUser(Full(user))
      }

      RewriteResponse("lost-sticky-note" :: Nil)
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

    case "register-casual-blogger" :: Nil => registerButton("Casual Blogger")
    case "register-influencer" :: Nil => registerButton("The Influencer")
    case "register-industry-leader" :: Nil => registerButton("Industry Leader")
  }

  def registerButton(planName: String) = {
    "button [data-plan-id]" #> Plan.find("name" -> planName).map(_._id.toString)
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

    var stripeToken = ""
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
      (plan.hasTrial_?.toString, plan._id.toString, plan.registrationTitle)
    }

    def createStripeCustomer(plan: Plan) = {
      if (stripeToken.trim.nonEmpty) {
        tryo(stripe.Customer.create(Map(
          "plan" -> plan.stripeId.getOrElse(""),
          "email" -> emailAddress,
          "card" -> stripeToken
        )))
      } else {
        tryo(stripe.Customer.create(Map(
          "plan" -> plan.stripeId.getOrElse(""),
          "email" -> emailAddress
        )))
      }
    }

    def generateSubscriptionForPlan(plan:Plan) = {
      Full(UserSubscription(plan._id, plan.price, plan.term))
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
              customer <- createStripeCustomer(plan)
            } yield {
              val firstSteps = Map(
                UserFirstStep.Keys.ConnectAnExternalService -> UserFirstStep.Steps.ConnectAnExternalService,
                UserFirstStep.Keys.CreateATab -> UserFirstStep.Steps.CreateATab,
                UserFirstStep.Keys.EmbedYourTab -> UserFirstStep.Steps.EmbedYourTab
              )

              val userActiveCard = customer.activeCard.map { card =>
                UserActiveCard(card.last4, card.`type`, card.expMonth, card.expYear)
              }

              User(emailAddress, User.hashPassword(requestedPassword),
                   Some(UserProfile(Some(firstName), Some(lastName), Some(organization))),
                   subscriptions = List(subscription), firstSteps = firstSteps,
                   stripeCustomerId = Some(customer.id),
                   activeCard = userActiveCard)
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

              loginResult

            case m =>
              logger.error("While registering account got: " + m)
              Alert("Something went wrong during account creation.")
          }

        case errors =>
          errors.foldLeft(Noop)(_ & _)
      }
    }

    val bind =
      ".plan-selection" #> selectPlans(planSelections, Empty, selectedPlan = _) &
      "#stripe-token" #> hidden(stripeToken = _, stripeToken) &
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
