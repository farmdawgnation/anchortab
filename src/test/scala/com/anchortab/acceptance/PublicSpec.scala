package com.anchortab.acceptance

import org.scalatest._
import org.scalatest.selenium._
import org.scalatest.concurrent._

case class RemotePublicSpec(testHost: String, testAuthorization: String) extends PublicSpecImpl

class PublicSpec extends PublicSpecImpl with AnchorTabSpec
trait PublicSpecImpl extends AcceptanceSpec {
  val testHost: String

  info("")
  info("As a public user")
  info("I want to be able to use the public Anchor Tab Site")
  info("So I can gain information about Anchor Tab")
  info("And log into my account.")
  info("")

  feature("Page Titles") {
    scenario("I load the Anchor Tab Landing Page", AcceptanceTest, PublicTest) {
      Given("I have navigated to the landing page")
      go to (testHost)

      When("it loads")

      Then("the title should be 'Anchor Tab'")
      pageTitle should be ("Anchor Tab")
    }

    scenario("I visit the Anchor Tab About Us Page", AcceptanceTest, PublicTest) {
      Given("I have navigated to the about us page")
      go to (testHost + "/about-us")

      When("it loads")

      Then("the title should be 'About Us | Anchor Tab'")
      pageTitle should be ("About Us | Anchor Tab")
    }
  }

  feature("Login") {
    scenario("Login details are invalid", AcceptanceTest, PublicTest) {
      Given ("I am on the landing page")
      go to (testHost)

      When("I enter invalid login credentials")
      textField(cssSelector(".email")).value = "awesome@sauce.com"
      cssSelector(".password").element.underlying.sendKeys("bacon")
      click on cssSelector(".submit")

      Then("The email and password fields should have the error CSS class")
      eventually {
        cssSelector(".email").element.attribute("class") should be (Some("email error"))
        cssSelector(".password").element.attribute("class") should be (Some("password error"))
      }
    }

    scenario("Login details are valid", AcceptanceTest, PublicTest) {
      Given("There is a valid user in the database")
      val (email, password) = validUser

      And("I am on the landing page")
      go to (testHost)

      When("I enter valid login credentials")
      textField(cssSelector(".email")).value = email
      cssSelector(".password").element.underlying.sendKeys(password)
      click on cssSelector(".submit")

      Then("I am redirected to the tab manager dashboard")
      eventually {
        currentUrl should be (testHost + "/manager/dashboard")
      }
    }
  }
}
