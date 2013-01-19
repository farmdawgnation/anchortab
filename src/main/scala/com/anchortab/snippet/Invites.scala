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

import com.anchortab.model.InviteCode

import org.bson.types.ObjectId

object inviteCode extends RequestVar[Box[InviteCode]](Empty)

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
  }

  def snippetHandlers: SnippetPF = {
    case "invites-list" :: Nil => invitesList
  }

  def invitesList = {
    val inviteCodes = InviteCode.findAll

    def inviteLink(inviteCode: InviteCode)(s: String) = {
      Alert(inviteCode.url)
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
      ".edit-invite [onclick]" #> onEvent((s:String) => Alert("HAI")) &
      ".delete-invite [onclick]" #> onEventIf("Delete this invite code?", deleteInviteCode(inviteCode) _)
    }
  }
}
