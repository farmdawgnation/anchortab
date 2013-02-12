package com.anchortab.acceptance

import org.scalatest._
import org.scalatest.selenium._
import org.scalatest.concurrent._
import org.scalatest.time._

trait PublicSpec extends FeatureSpec with GivenWhenThen
    with Chrome with BeforeAndAfterAll with ShouldMatchers
    with Eventually with IntegrationPatience {
  def host: String

  info("As a public user")
  info("I want to be able to use the public Anchor Tab Site")
  info("So I can gain information about Anchor Tab")
  info("And log into my account.")

  implicitlyWait(Span(2, Seconds))

  feature("Page Titles") {
    scenario("I load the Anchor Tab Landing Page", AcceptanceTest) {
      Given("I have navigated to the landing page")
      go to (host)

      When("it loads")

      Then("the title should be 'Anchor Tab'")
      pageTitle should be ("Anchor Tab")
    }

    scenario("I visit the Anchor Tab About Us Page", AcceptanceTest) {
      Given("I have navigated to the about us page")
      go to (host + "/about-us")

      When("it loads")

      Then("the title should be 'About Us | Anchor Tab'")
      pageTitle should be ("About Us | Anchor Tab")
    }
  }

  feature("Login") {
    scenario("Login details are invalid", AcceptanceTest) {
      Given ("I am on the landing page")
      go to (host)

      When("I enter invalid login credentials")
      textField(cssSelector(".email")).value = "awesome@sauce.com"
      cssSelector(".password").element.underlying.sendKeys("bacon")
      click on cssSelector(".submit")

      // Give the AJAX request time.
      Thread.sleep(500)

      Then("The email and password fields should have the error CSS class")
      cssSelector(".email").element.attribute("class") should be (Some("email error"))
      cssSelector(".password").element.attribute("class") should be (Some("password error"))
    }
  }
}
