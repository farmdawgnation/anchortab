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

import net.liftweb.util._
  import Helpers._

import com.anchortab._
  import model._

object AcceptanceTest extends Tag("com.anchortab.acceptance.AcceptanceTest")

object PublicTest extends Tag("com.anchortab.acceptance.PublicTest")

trait AcceptanceSpec extends FeatureSpec with GivenWhenThen 
    with Chrome with BeforeAndAfterAll with ShouldMatchers 
    with Eventually with IntegrationPatience {
  def generateValidUser = {
    val email = randomString(32)
    val password = randomString(32)

    User(email, User.hashPassword(password)).save

    (email, password)
  }
}

class AnchorTabAcceptanceSpec extends AcceptanceSpec with PublicSpec {
  private var server : Server       = null
  private val GUI_PORT              = 8080
  protected val host                  = "http://local.anchortab.com"

  implicitlyWait(Span(2, Seconds))

  override def beforeAll() {
    // Setting up the jetty instance which will be running the
    // GUI for the duration of the tests
    server  = new Server(GUI_PORT)
    val context = new WebAppContext()
    context.setServer(server)
    context.setContextPath("/")
    context.setWar("src/main/webapp")
    server.setHandler(context)
    server.start()
  }

  override def afterAll() {
    server.stop()
  }
}
