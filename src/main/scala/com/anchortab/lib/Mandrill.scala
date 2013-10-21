package com.anchortab.lib

import net.liftweb._
  import common._
  import actor._
  import mongodb._
  import util.Helpers._
  import json._
    import ext._
    import JsonDSL._
    import Extraction._
  import util._
  import http._

import dispatch._, Defaults._
import com.ning.http.client.{Request, RequestBuilder, Response}

object Mandrill extends Loggable {
  implicit val formats = DefaultFormats

  private val key = "cpy4Hbh3VICrlVA5cpq_cw"
  private val endpointHost = "mandrillapp.com"
  private val endpointVersion = "1.0"

  case class MandrillTo(email: String, name: Option[String] = None)
  case class MandrillMessage(subject: String, from_email: String, to: List[MandrillTo],
    from_name: Option[String] = None, html: Option[String] = None,
    text: Option[String] = None)

  trait MandrillApiCall {
    def uri(requestBuilder: dispatch.Req): dispatch.Req
  }
  case class SendMandrillMessage(message: MandrillMessage, async: Boolean = false) extends MandrillApiCall {
    def uri(requestBuilder: dispatch.Req) = requestBuilder / "messages" / "send.json"
  }

  case class CodeResponse(code: Int)
  protected object AsCodeResponse extends (Response => CodeResponse) {
    def apply(r:Response) = {
      CodeResponse(
        r.getStatusCode()
      )
    }
  }

  case class MandrillResponse(status: String)

  def run(apiCall: MandrillApiCall) = {
    val postJson = decompose(apiCall) match {
      case obj:JObject =>
        obj ~
        ("key" -> key)

      case _ => JObject(Nil)
    }

    val requestBody = compact(render(postJson))
    val request = (apiCall.uri(host(endpointHost) / "api" / endpointVersion)).secure <<
      requestBody

    val response = Http(request > AsCodeResponse).either

    response() match {
      case Right(CodeResponse(200)) => // All good.
      case Right(CodeResponse(code)) => logger.error("Mandrill returned code: " + code)
      case Left(dispatchError) => logger.error("Dispatch error: " + dispatchError)
    }
  }
}
