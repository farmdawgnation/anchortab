package com.anchortab.snippet

import scala.xml._

import net.liftweb._
  import sitemap._
    import Loc._
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
  import mongodb.BsonDSL._

import com.anchortab._
import com.anchortab.model.{InviteCode, Plan}

import org.bson.types.ObjectId

object Invites extends Loggable {
  val acceptInviteMenu =
    Menu.param[InviteCode]("Accept Invite", Text("Accept Invite"), inviteForCode(_), _.code) /
    "accept-invite" / * >>
    TemplateBox(() => Templates("register" :: Nil)) >>
    Authentication.ifNotLoggedIn

  val invitesListMenu = Menu.i("Invites") / "admin" / "invites" >>
    Authentication.ifLoggedIn >>
    Authentication.ifAdmin

  val invitesNewMenu = Menu.i("New Invite") / "admin" / "invites" / "new" >>
    TemplateBox(() => Templates("admin" :: "invite" :: "form" :: Nil)) >>
    Authentication.ifLoggedIn >>
    Authentication.ifAdmin

  val inviteEditMenu =
    Menu.param[InviteCode]("Edit Invite", Text("Edit Invite"), InviteCode.find(_), _._id.toString) /
    "admin" / "invite" / * >>
    TemplateBox(() => Templates("admin" :: "invite" :: "form" :: Nil)) >>
    Authentication.ifLoggedIn >>
    Authentication.ifAdmin

  val menus =
    acceptInviteMenu ::
    invitesListMenu ::
    invitesNewMenu ::
    inviteEditMenu ::
    Nil

  def inviteForCode(userInviteCode: String) = {
    for {
      invite <- InviteCode.findAll("code" -> userInviteCode).headOption
        if invite.numberOfUsesAvailable.map(_ > invite.numberOfUses) getOrElse true
    } yield {
      invite
    }
  }

  def snippetHandlers: SnippetPF = {
    case "invites-list" :: Nil => invitesList
    case "new-invite-button" :: Nil => newInviteButton
    case "invite-form" :: Nil => inviteForm
  }

  def inviteForm = {
    val requestInviteCode = inviteEditMenu.currentValue openOr InviteCode()

    var theCode = requestInviteCode.code
    var maxUses = requestInviteCode.numberOfUsesAvailable.map(_.toString) getOrElse ""
    var validityStart = ""
    var validityEnd = ""
    var plan: Option[Plan] = requestInviteCode.forPlan

    def saveInviteCode() = {
      val numUses = {
        maxUses match {
          case "" => None
          case x => Some(x.toInt)
        }
      }

      requestInviteCode.copy(
        numberOfUsesAvailable = numUses,
        forPlanId = plan.map(_._id)
      ).save

      RedirectTo("/admin/invites")
    }

    val bind =
      ".current-code" #> text(theCode, theCode = _) &
      ".max-uses" #> text(maxUses, maxUses = _) &
      ".validity-start" #> text(validityStart, validityStart = _) &
      ".valitiy-end" #> text(validityEnd, validityEnd = _) &
      ".plan" #> selectObj[Option[Plan]](
        List((None, "None")) ++ Plan.findAll.map(p => (Some(p), p.name)),
        Full(plan),
        plan = _
      ) &
      ".submit" #> ajaxSubmit("Save Invite Code", saveInviteCode _)

    "form" #> { ns: NodeSeq =>
      ajaxForm(
        bind(ns)
      )
    }
  }

  def newInviteButton = {
    ".new-invite [onclick]" #> onEvent((s: String) => RedirectTo("/admin/invites/new"))
  }

  def invitesList = {
    val inviteCodes = InviteCode.findAll

    def inviteLink(inviteCode: InviteCode)(s: String) = {
      Alert(inviteCode.url)
    }

    def editInviteCode(inviteCode: InviteCode)(s: String) = {
      RedirectTo("/admin/invite/" + inviteCode._id.toString)
    }

    def deleteInviteCode(inviteCode: InviteCode)(s: String) = {
      inviteCode.delete

      Reload
    }

    ".invite-row" #> inviteCodes.map { inviteCode =>
      ".invite-code *" #> inviteCode.code &
      ".accepts *" #> inviteCode.numberOfUses &
      ".accepts-remain *" #> (inviteCode.numberOfUsesAvailable.map(_ - inviteCode.numberOfUses).map(_.toString) getOrElse "unlimited") &
      ".invite-link [onclick]" #> onEvent(inviteLink(inviteCode) _) &
      ".edit-invite [onclick]" #> onEvent(editInviteCode(inviteCode) _) &
      ".delete-invite [onclick]" #> onEventIf("Delete this invite code?", deleteInviteCode(inviteCode) _)
    }
  }
}
