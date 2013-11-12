package com.anchortab.acceptance

import org.scalatest._
import org.scalatest.selenium._
import org.scalatest.concurrent._
import org.scalatest.time._

import com.thoughtworks.selenium.Wait

import org.eclipse.jetty.server.handler.ContextHandler
import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.server.{Handler, Server}
import org.eclipse.jetty.webapp.WebAppContext

import org.scalatest.Tag

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util._
  import Helpers._
import net.liftweb.json._
  import JsonDSL._
  import Extraction._
  import Serialization._

import com.anchortab._
  import model._

import dispatch._, Defaults._

object AcceptanceTest extends Tag("com.anchortab.acceptance.AcceptanceTest")

object PublicTest extends Tag("com.anchortab.acceptance.PublicTest")

trait AcceptanceSpec extends FeatureSpec with GivenWhenThen 
    with Firefox with BeforeAndAfterAll with ShouldMatchers
    with Eventually with IntegrationPatience {

  val testHost: String
  val testAuthorization: String

  var validUser: (String, String) = ("", "")
  var validUserObjectId = ""

  override def beforeAll() {
    implicit val formats = DefaultFormats

    val email = randomString(32) + "@test.local"
    val password = randomString(32)
    validUser = (email, password)

    val authorization = Map(
      "Authorization" -> ("Bearer " + testAuthorization)
    )
    val requestJson =
      ("email" -> email) ~
      ("password" -> password)
    val requestBody = compact(render(requestJson))
    val request = url(testHost) / "api" / "v1" / "admin" / "users" << requestBody <:< authorization
    val response = Http(request OK as.String)

    // Record the object Id
    val responseJson = response()

    for {
      responseJvalue <- tryo(Serialization.read[JValue](new String(responseJson)))
      id <- tryo(responseJvalue \ "id").map(_.extract[String])
    } {
      validUserObjectId = id
    }
  }

  override def afterAll() {
    // Clean house
    val authorization = Map(
      "Authorization" -> ("Bearer " + testAuthorization)
    )
    val request = (url(testHost) / "api" / "v1" / "admin" / "user" / validUserObjectId).DELETE <:< authorization
    val response = Http(request OK as.String)

    // Wait for completion
    response()
  }
}

trait AnchorTabSpec extends AcceptanceSpec {
  val testHost            = "http://local.anchortab.com"
  val testAuthorization   = "NZDTCNLTJVINNUW3RMEQTPWFW0VOBU52"

  implicitlyWait(Span(2, Seconds))

  override def beforeAll() {
    super.beforeAll()
  }

  override def afterAll() {
    super.afterAll()
    quit()
  }
}
