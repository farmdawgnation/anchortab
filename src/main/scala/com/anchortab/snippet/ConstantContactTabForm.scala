package com.anchortab.snippet

import scala.collection.JavaConverters._

import net.liftweb._
  import common._

import com.anchortab.model._
import com.anchortab.constantcontact.model.ContactLists._

import com.newrelic.api.agent.NewRelic

trait ConstantContactTabForm extends Loggable {
  val constantContactLists = {
    for {
      user <- currentUser.is
      credentials <- user.credentialsFor("Constant Contact")
    } yield {
      implicit val accessToken = credentials.serviceCredentials.get("token") getOrElse ""
      ContactList.findAll match {
        case Full(list) => list
        case Failure(msg, _, _) =>
          NewRelic.noticeError("Constant Contact List List Error", Map(
            "error message" -> msg
          ).asJava)

          logger.error(msg)
          Nil
        case _ => Nil
      }
    }
  } openOr {
    Nil
  }
}
