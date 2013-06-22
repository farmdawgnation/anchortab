package com.anchortab.campaignmonitor

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

import net.liftweb._
  import common._
  import util._
    import Helpers._
  import json._
    import Extraction._
  import mongodb.BsonDSL._

import com.anchortab.model._

import org.bson.types.ObjectId

import org.joda.time._

import com.createsend._
import com.createsend.util._
import com.createsend.models._
import com.createsend.models.subscribers._

case class CampaignMonitorListBasics(id: String, name: String)

/**
 * A lightweight wrapper around Campaign Monitor's createsend-java api
 * that makes exceptions become Failures and abstracts away any nasty
 * that would normally be involved in interacting with a Java API wrapper.
**/
object CampaignMonitor {
  val serviceIdentifier = "Campaign Monitor"

  private val clientId = Props.getInt("campaignmonitor.clientId") ?~ "Couldn't find Campaign Monitor client ID."
  private val clientSecret = Props.get("campaignmonitor.clientSecret") ?~ "Couldn't find Campaign Monitor client secret."
  private val redirectUri = Props.get("campaignmonitor.redirectUri") ?~ "Couldn't find Campaign Monitor redirect URI."
  private val scope = Props.get("campaignmonitor.scope") ?~ "Couldn't find Campaign Monitor scope."

  def oauthAuthorizeUrl = {
    for {
      clientId <- clientId
      redirectUri <- redirectUri
      scope <- scope
      oauthUrl <- tryo(General.getAuthorizeUrl(
        clientId,
        redirectUri,
        scope,
        ""
      ))
    } yield {
      oauthUrl
    }
  }

  def exchangeToken(token: String) = {
    for {
      clientId <- clientId
      clientSecret <- clientSecret
      redirectUri <- redirectUri
      tokenDetails <- tryo(General.exchangeToken(
        clientId,
        clientSecret,
        redirectUri,
        token
      ))
    } yield {
      tokenDetails
    }
  }

  def refreshToken(accessToken: String, refreshToken: String): Box[OAuthTokenDetails] = {
    val general = new General(new OAuthAuthenticationDetails(accessToken, refreshToken))

    for {
      refreshTokenResult <- tryo(general.refreshToken)
    } yield {
      refreshTokenResult
    }
  }

  def getLists(accessToken: String, refreshToken: String) = {
    val auth = new OAuthAuthenticationDetails(accessToken, refreshToken)
    val cmGeneral = new General(auth)

    for {
      clientIds <- tryo(cmGeneral.getClients().map(_.ClientID))
      clients = clientIds.map(new Clients(auth, _))
      listsBasics <- tryo(clients.map(_.lists()).flatten)
    } yield {
      listsBasics.map { listBasics =>
        CampaignMonitorListBasics(listBasics.ListID, listBasics.Name)
      }
    }
  }

  def addSubscriber(accessToken: String, refreshToken: String, listId: String, email: String, name: Option[String]): Box[String] = {
    val auth = new OAuthAuthenticationDetails(accessToken, refreshToken)
    val cmSubscribers = new Subscribers(auth, listId)

    val subscriber = new SubscriberToAdd()
    subscriber.EmailAddress = email
    subscriber.ListID = listId

    name.foreach { name =>
      subscriber.Name = name
    }

    for {
      result <- tryo(cmSubscribers.add(subscriber))
    } yield {
      result
    }
  }

  def removeSubscriber(accessToken: String, refreshToken: String, listId: String, email: String) = {
    val auth = new OAuthAuthenticationDetails(accessToken, refreshToken)
    val cmSubscribers = new Subscribers(auth, listId)

    for {
      result <- tryo(cmSubscribers.unsubscribe(email))
    } yield {
      result
    }
  }
}

object CampaignMonitorCredentialsHelper extends CampaignMonitorCredentialsHelper
trait CampaignMonitorCredentialsHelper extends Loggable {
  def updateCredentials(userId: ObjectId, accessToken: String, refreshToken: String, expiresAt: DateTime) = {
    implicit val formats = User.formats

    User.update(
      (
        ("_id" -> userId) ~
        ("serviceCredentials.serviceName" -> CampaignMonitor.serviceIdentifier)
      ),
      ("$set" -> (
        ("serviceCredentials.$.serviceCredentials.accessToken" -> accessToken) ~
        ("serviceCredentials.$.serviceCredentials.refreshToken" -> refreshToken) ~
        ("serviceCredentials.$.serviceCredentials.expiresAt" -> decompose(expiresAt))
      ))
    )
  }

  def doWithNewAccessCredentials[T](userId: ObjectId, accessToken: String, refreshToken: String, doSomething: (String, String)=>Box[T]): Box[T] = {
    val tokenRefreshResult = for {
      newToken <- CampaignMonitor.refreshToken(accessToken, refreshToken)
      expiresAt = (new DateTime()).plusSeconds(newToken.expires_in)
      updateCredentialsUnit = updateCredentials(userId, accessToken, refreshToken, expiresAt)
      resultOfSomething <- doSomething(newToken.access_token, newToken.refresh_token)
    } yield {
      resultOfSomething
    }

    tokenRefreshResult match {
      case result @ Full(_) =>
        logger.info("Refreshed CM token.")
        result

      case fail @ Failure(msg, _, _) =>
        logger.error("Error refreshing CM token: " + msg)
        fail

      case Empty =>
        logger.error("Empty refreshing CM token.")
        Empty
    }
  }

  def withAccessCredentials[T](userId: ObjectId)(doSomething: (String, String)=>Box[T]): Box[T] = {
    {
      for {
        user <- (User.find(userId):Box[User])
        credentials <- user.credentialsFor(CampaignMonitor.serviceIdentifier)
        accessToken <- credentials.serviceCredentials.get("accessToken")
        refreshToken <- credentials.serviceCredentials.get("refreshToken")
        expiresAt <- credentials.serviceCredentials.get("expiresAt")
      } yield {
        if (DateTime.parse(expiresAt) isBeforeNow) {
          doWithNewAccessCredentials(userId, accessToken, refreshToken, doSomething)
        } else {
          doSomething(accessToken, refreshToken)
        }
      }
    } match {
      // We unwrap fulls.
      case Full(t) => t

      case fail @ Failure(msg, _, _) =>
        logger.error("Returning a failure from Campaign Monitor: " + msg)
        fail

      case _ => Empty
    }
  }
}
