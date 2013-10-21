package com.anchortab.mailchimp

import net.liftweb._
  import common._
  import json._
  import util._
    import Helpers._

import dispatch._, Defaults._
import com.ning.http.client.{Request, RequestBuilder, Response}

object MailchimpOAuth {
  private val oauthHost = "login.mailchimp.com"

  private val redirectUrl = Props.get("mailchimp.redirectUrl") openOr "http://local.anchortab.com/oauth2/mailchimp"
  private val clientId = Props.get("mailchimp.clientId") openOr "885152876004"
  private val clientSecret = Props.get("mailchimp.clientSecret") openOr "f3e27f917c58a5e44f36ce2ffcbe79c9"

  def oAuthAuthorizeUrl = {
    (host(oauthHost) / "oauth2" / "authorize" <<?
      Map("response_type" -> "code", "client_id" -> clientId, "redirect_uri" -> redirectUrl)
    ).secure.toRequest.getRawUrl
  }

  case class MailChimpOAuthToken(access_token: String, expires_in: Int, scope: String)
  case class MailChimpOAuthMetadata(dc: String, login_url: String, api_endpoint: String)
  case class CodeJsonResponse(code:Int, json:Box[JValue])

  protected object AsCodeJsonResponse extends (Response => CodeJsonResponse) {
    def apply(r:Response) = {
      CodeJsonResponse(
        r.getStatusCode(),
        tryo(as.lift.Json(r))
      )
    }
  }

  protected def retrieveAccessTokenForCode(code: String) = {
    implicit val formats = DefaultFormats

    val request = (host(oauthHost) / "oauth2" / "token").secure <<
      Map("grant_type" -> "authorization_code", "client_id" -> clientId,
        "client_secret" -> clientSecret, "redirect_uri" -> redirectUrl,
        "code" -> code)

    val response = Http(request > AsCodeJsonResponse).either

    response() match {
      case Right(CodeJsonResponse(200, Full(accessTokenJson))) =>
        for {
          accessToken <- tryo(accessTokenJson.extract[MailChimpOAuthToken])
        } yield {
          accessToken.access_token
        }

      case Right(CodeJsonResponse(code, _)) =>
        Failure("Something went wrong retrieving access token.")

      case Left(error) => Failure("Dispatch error.")
    }
  }

  protected def retrieveDatacenterForAccessToken(accessToken: String) = {
    implicit val formats = DefaultFormats

    val request = (host(oauthHost) / "oauth2" / "metadata").secure <:<
      Map("Authorization" -> ("OAuth " + accessToken))

    val response = Http(request > AsCodeJsonResponse).either

    response() match {
      case Right(CodeJsonResponse(200, Full(accessTokenMetadataJson))) =>
        for {
          accessTokenMetadata <- tryo(accessTokenMetadataJson.extract[MailChimpOAuthMetadata])
        } yield {
          accessTokenMetadata.dc
        }

      case Right(CodeJsonResponse(code, _)) =>
        Failure("Something went wrong retrieving metadata: " + code)

      case Left(error) => Failure("Dispatch error: " + error)
    }
  }

  def retrieveAccessTokenAndDcForCode(code: String) = {
    for {
      accessToken <- retrieveAccessTokenForCode(code)
      dc <- retrieveDatacenterForAccessToken(accessToken)
    } yield {
      accessToken + "-" + dc
    }
  }
}
