package com.anchortab.snippet

import com.anchortab._

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

import com.anchortab.model.{InviteCode, Plan}

import org.bson.types.ObjectId

object inviteCode extends RequestVar[Box[InviteCode]](Empty)
object requestInviteId extends RequestVar[Box[ObjectId]](Empty)

object Invites extends Loggable {
  def statelessRewrite: RewritePF = {
    case RewriteRequest(ParsePath("accept-invite" :: userInviteCode :: Nil, _, _, _), _, _) =>
      for {
        invite <- InviteCode.findAll("code" -> userInviteCode)
          if invite.numberOfUsesAvailable.map(_ > invite.numberOfUses) getOrElse true
      } {
        inviteCode(Full(invite))
      }

      RewriteResponse("register" :: Nil)

    case RewriteRequest(ParsePath("admin" :: "invites" :: "new" :: Nil, _, _, _), _, _) =>
      RewriteResponse("admin" :: "invite" :: "form" :: Nil)

    case RewriteRequest(ParsePath("admin" :: "invite" :: inviteId :: "edit" :: Nil, _, _, _), _, _) =>
      {
        for {
          inviteObjectId <- tryo(new ObjectId(inviteId))
        } yield {
          requestInviteId(Full(inviteObjectId))

          RewriteResponse("admin" :: "invite" :: "form" :: Nil)
        }
      } openOr {
        S.redirectTo("/admin/invites")
      }
  }

  def snippetHandlers: SnippetPF = {
    case "invites-list" :: Nil => invitesList
    case "new-invite-button" :: Nil => newInviteButton
    case "invite-form" :: Nil => inviteForm
  }

  def inviteForm = {
    val requestInviteCode = requestInviteId.is.flatMap(InviteCode.find(_)) openOr InviteCode()

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
      RedirectTo("/admin/invite/" + inviteCode._id.toString + "/edit")
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
