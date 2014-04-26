package com.anchortab.snippet

import scala.collection.JavaConverters._

import net.liftweb._
  import common._

import com.anchortab.model._
import com.anchortab.campaignmonitor._

import com.newrelic.api.agent.NewRelic

trait CampaignMonitorTabForm extends Loggable {
  val campaignMonitorAuthorized_? = {
    for {
      user <- currentUser.is
      credentials <- user.credentialsFor(CampaignMonitor.serviceIdentifier)
    } yield {
      true
    }
  } openOr {
    false
  }

  val campaignMonitorLists = {
    val lists = {
      for {
        session <- userSession.is
        lists <- CampaignMonitorCredentialsHelper.withAccessCredentials(session.userId) { (accessToken, refreshToken) =>
          CampaignMonitor.getLists(accessToken, refreshToken)
        }
      } yield {
        lists.toList
      }
    }

    lists match {
      case Full(cmLists) => cmLists

      case Failure(msg, _, _) =>
        NewRelic.noticeError("Campaign Monitor List List Error", Map(
          "error message" -> msg
        ).asJava)

        Nil

      case _ => Nil
    }
  }
}
