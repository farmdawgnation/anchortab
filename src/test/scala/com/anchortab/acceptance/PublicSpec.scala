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

class PublicSpec extends FeatureSpec with GivenWhenThen 
    with HtmlUnit with BeforeAndAfterAll with ShouldMatchers 
    with Eventually with IntegrationPatience {
  private var server : Server       = null
  private val GUI_PORT              = 8080
  private var host                  = "http://local.anchortab.com"

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

  info("As a public user")
  info("I want to be able to use the public Anchor Tab Site")
  info("So I can gain information about Anchor Tab")
  info("And log into my account.")

  implicitlyWait(Span(2, Seconds))

  feature("Landing Page") {
    scenario("I load the Anchor Tab Landing Page") {
      When("I visit the landing page")
      go to (host)

      Then("the title should be 'Anchor Tab'")
      pageTitle should be ("Anchor Tab")
    }
  }
}
