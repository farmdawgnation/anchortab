package com.anchortab.campaignmonitor

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

import net.liftweb._
  import common._
  import util._
    import Helpers._

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

  def addSubscriber(accessToken: String, refreshToken: String, listId: String, email: String): Box[String] = {
    val auth = new OAuthAuthenticationDetails(accessToken, refreshToken)
    val cmSubscribers = new Subscribers(auth, listId)

    val subscriber = new SubscriberToAdd()
    subscriber.EmailAddress = email
    subscriber.ListID = listId

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
