package com.anchortab.actor

import scala.xml.NodeSeq

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

import dispatch._
import com.ning.http.client.{Request, RequestBuilder, Response}

import com.anchortab.model._

import org.bson.types.ObjectId

sealed trait EmailActorMessage
case class SendWelcomeEmail(userEmail: String) extends EmailActorMessage

object EmailActor extends LiftActor with Loggable {
  implicit val formats = DefaultFormats

  protected object Mandrill {
    private val key = "cpy4Hbh3VICrlVA5cpq_cw"
    private val endpointHost = "mandrillapp.com"
    private val endpointVersion = "1.0"

    case class MandrillTo(email: String, name: Option[String] = None)
    case class MandrillMessage(subject: String, from_email: String, to: List[MandrillTo],
      from_name: Option[String] = None, html: Option[String] = None,
      text: Option[String] = None)

    trait MandrillApiCall {
      def uri: String
    }
    case class SendMandrillMessage(message: MandrillMessage, async: Boolean = false) extends MandrillApiCall {
      val uri = "/messages/send.json"
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
      val request = (host(endpointHost) / "api" / endpointVersion / apiCall.uri).secure <<
        requestBody

      val response = Http(request > AsCodeResponse).either

      response() match {
        case Right(CodeResponse(200)) => // All good.
        case Right(CodeResponse(code)) => logger.error("Mandrill returned code: " + code)
        case Left(dispatchError) => logger.error("Dispatch error: " + dispatchError)
      }
    }
  }

  val fromEmail = "hello@anchortab.com"
  val fromName = "Anchor Tab"

  val welcomeEmailSubject = "Welcome to Anchor Tab!"
  val welcomeEmailTemplate = 
    Templates("emails-hidden" :: "welcome-email" :: Nil) openOr NodeSeq.Empty

  def messageHandler = {
    case SendWelcomeEmail(userEmail) =>
      val sendMandrillMessage = Mandrill.SendMandrillMessage(
        Mandrill.MandrillMessage(welcomeEmailSubject, fromEmail,
          Mandrill.MandrillTo(userEmail) :: Nil,
          Some(fromName),
          html = Some(welcomeEmailTemplate.toString))
      )

      Mandrill.run(sendMandrillMessage)

    case _ =>
  }
}
