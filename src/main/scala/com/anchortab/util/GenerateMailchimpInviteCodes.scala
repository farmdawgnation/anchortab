package com.anchortab.util

import scala.collection.JavaConverters._

import net.liftweb.util.Helpers._

import com.ecwid.mailchimp._
  import method._
    import list._

import com.anchortab.model._

import org.bson.types.ObjectId

object GenerateMailchimpInviteCodes {
  def run(key: String, listId: String) = {
    val mcClient = new MailChimpClient

    val listMembersMethod = new ListMembersMethod
    listMembersMethod.apikey = key
    listMembersMethod.id = listId
    listMembersMethod.status = MemberStatus.subscribed

    val listMembersResult = mcClient.execute(listMembersMethod)
    val planId = new ObjectId("50b6f30a0364aafe5ac6a5a1")

    for {
      listMember <- listMembersResult.data.asScala
    } yield {
      val inviteCode = InviteCode(numberOfUsesAvailable = Some(1), forPlanId = Some(planId))
      inviteCode.save

      val newMergeVars = new MergeVars(inviteCode.code)

      val listUpdateMemberMethod = new ListUpdateMemberMethod
      listUpdateMemberMethod.apikey = key
      listUpdateMemberMethod.id = listId
      listUpdateMemberMethod.email_address = listMember.email
      listUpdateMemberMethod.merge_vars = newMergeVars

      tryo(mcClient.execute(listUpdateMemberMethod))
    }
  }
}
