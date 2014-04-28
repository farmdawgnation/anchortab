package com.anchortab.snippet

import scala.collection.JavaConverters._

import net.liftweb._
  import common._

import com.anchortab.model._

import com.ecwid.mailchimp._
  import method.list._

import com.newrelic.api.agent.NewRelic

trait MailChimpTabForm {
  val mailChimpAuthorized_? = {
    for {
      user <- currentUser.is
      credentials <- user.credentialsFor("Mailchimp")
    } yield {
      true
    }
  } openOr {
    false
  }

  val mailchimpLists = {
    val lists = {
      for {
        user <- currentUser.is
        credentials <- user.credentialsFor("Mailchimp")
        token <- credentials.serviceCredentials.get("token")
      } yield {
        val mcClient = new MailChimpClient
        val listsMethod = new ListsMethod
        listsMethod.apikey = token

        mcClient.execute(listsMethod)
      }
    } map(_.data.asScala.toList)

    lists match {
      case Full(mcLists) => mcLists

      case Failure(msg, _, _) =>
        NewRelic.noticeError("MailChimp List List Error", Map(
          "error message" -> msg
        ).asJava)

        Nil

      case _ => Nil
    }
  }
}
