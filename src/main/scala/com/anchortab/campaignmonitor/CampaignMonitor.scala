package com.anchortab.campaignmonitor

import net.liftweb._
  import common._
  import util._
    import Helpers._

import com.createsend._

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
}
