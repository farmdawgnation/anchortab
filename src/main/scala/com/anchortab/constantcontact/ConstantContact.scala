package com.anchortab.constantcontact

import net.liftweb._
  import common._
  import json._
  import util._
    import Helpers._

import dispatch._
import com.ning.http.client.{Request, RequestBuilder, Response}

object ConstantContact {
  private val endpointBase = "api.constantcontact.com"
  private val endpointVersion = "v2"

  private val oauthBase = "oauth2.constantcontact.com"

  private val clientId = "16589cf8-86ae-4fda-82c6-9b4111d3ff98"
  private val clientSecret = "ea69bae4fcf349fd9d413baed8e362c6"
  private val redirectUrl = "https://anchortab.com/oauth/constant-contact"

  case class ConstantContactError(error_key: String, error_message: String)

  case class CodeJsonResponse(code:Int, json:Box[JValue])

  protected object AsCodeJsonResponse extends (Response => CodeJsonResponse) {
    def apply(r:Response) = {
      CodeJsonResponse(
        r.getStatusCode(),
        tryo(as.lift.Json(r))
      )
    }
  }

  case class OAuth2Token(access_token:String, expires_in:Long, token_type:String)

  def oauthAuthorizeUrl = {
    (host(oauthBase) / "oauth2" / "oauth" / "siteowner" / "authorize" <<?
      Map("response_type" -> "code", "client_id" -> clientId, "redirect_uri" -> redirectUrl)).secure.build.getRawUrl
  }

  def retrieveAccessTokenForCode(code:String) = {
    implicit val formats = DefaultFormats
    implicit val accessToken = "" // Don't need this for oauth

    val tokenResponse = runRequest {
      (host(oauthBase) / "oauth2" / "oauth" / "token") <<?
        Map("grant_type" -> "authorization_code", "client_id" -> clientId, "client_secret" -> clientSecret, "code" -> code,
            "redirect_uri" -> redirectUrl)
    }

    for {
      tokenJson <- tokenResponse
      token <- tryo(tokenJson.extract[OAuth2Token])
    } yield {
      token.access_token
    }
  }

  private def runRequest(request:RequestBuilder)(implicit accessToken:String) : Box[JValue] = {
    val response = Http(request.secure > AsCodeJsonResponse).either

    // For the promise to materialize and generate a result based on what we got
    // back from dispatch/the remote service.
    response() match {
      case Right(CodeJsonResponse(code, Full(json))) if code < 300 => Full(json)

      case Right(CodeJsonResponse(code, parseError:Failure)) if code < 300 =>
        Failure("ConstantContact response was not valid JSON.", Empty, parseError)

      case Right(CodeJsonResponse(401, _)) => Failure("Authentication with ConstantContact failed.")

      case Right(CodeJsonResponse(code, Full(json))) =>
        implicit val formats = DefaultFormats
        val errorList = json.extract[List[ConstantContactError]]
        val errorStrings = errorList.map(_.error_message)

        Failure("ConstantContact returned code " + code + " with errors: " + errorStrings)

      case Right(CodeJsonResponse(code, _)) => Failure("ConstantContact returned code: " + code)

      case Left(error) => Failure("Dispatch error: " + error)
    }
  }

  private[constantcontact] def get(resource:String, params:Map[String,String] = Map.empty)(implicit accessToken:String) : Box[JValue] = {
    runRequest {
      host(endpointBase) / endpointVersion / resource <<? (params + ("access_token" -> accessToken))
    }
  }

  private[constantcontact] def post(resource:String, body:JValue)(implicit accessToken:String) : Box[JValue] = {
    runRequest {
      val requestBody = compact(render(body))
      host(endpointBase) / endpointVersion / resource << requestBody <<? Map("access_token" -> accessToken)
    }
  }

  private[constantcontact] def put(resource:String, body:JValue)(implicit accessToken:String) : Box[JValue] = {
    runRequest {
      val requestBody = compact(render(body))
      (host(endpointBase) / endpointVersion / resource).PUT.setBody(requestBody) <<? Map("access_token" -> accessToken)
    }
  }

  private[constantcontact] def delete(resource:String)(implicit accessToken:String) : Box[JValue] = {
    runRequest {
      (host(endpointBase) / endpointVersion / resource).DELETE <<? Map("access_token" -> accessToken)
    }
  }
}
