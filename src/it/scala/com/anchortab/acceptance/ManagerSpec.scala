package com.anchortab.acceptance

import org.scalatest._
import org.scalatest.selenium._

import net.liftweb.util.Helpers._

//case class RemoteManagerSpec(testHost: String, testAuthorization: String) extends PublicSpecImpl

class ManagerSpec extends AnchorTabSpec with ManagerSpecImpl
trait ManagerSpecImpl extends AcceptanceSpec {
  val testHost: String

  val managerUrl = testHost + "/manager"

  val manageTabsUrl = managerUrl + "/tabs"
  val newTabsUrl = managerUrl + "/tabs/new"
  def editTabUrl(tabId: String) = managerUrl + "/tab/" + tabId + "/edit"

  val profileUrl = managerUrl + "/account"

  val connectedServicesUrl = managerUrl + "/services"

  override def beforeAll() {
    super.beforeAll()

    val (email, password) = validUser

    go to (testHost)
    textField(cssSelector(".email")).value = email
    cssSelector(".password").element.underlying.sendKeys(password)
    click on cssSelector(".submit")

    Thread.sleep(500)
  }

  info("")
  info("As a registered user")
  info("I want to be able to use the Tab Manager")
  info("So I can manager by tabs and my account.")
  info("")

  feature("Tab Management") {
    scenario("User views Manage Tabs with no tabs on account") {
      Given("I am on the tab manager homepager as a user with no tabs.")
      go to (managerUrl)

      When("I click the 'Manage Tabs' item in the navigation")
      click on cssSelector("nav .manage-tabs > a")

      Then("I am on the 'Manage Tabs' page")
      eventually {
        currentUrl should be (manageTabsUrl)
      }

      And("The 'Manage Tabs' navigation item is highlighted")
      cssSelector("nav .manage-tabs").element.attribute("class") should be (Some("manage-tabs selected"))

      And("The no tabs message is displayed on the page")
      cssSelector("tr.empty-list td").element.text should be ("You currently have no tabs.")
    }

    scenario("User clicks new tab button") {
      Given("I am on the tab management screen")
      go to (manageTabsUrl)

      When("I click the New Tab button")
      click on cssSelector(".new-tab-button")

      Then("I am taken to the new tab form")
      eventually {
        currentUrl should be (newTabsUrl)
      }
    }

    scenario("User creates a new tab") {
      val tabName = randomString(32)

      Given("I am at the new tab form")
      go to (newTabsUrl)

      When("I complete the New Tab form")
      textField("tab-name").value = tabName
      textField("custom-text").value = randomString(32)

      And("click the save tab button")
      click on cssSelector("input.submit")

      Then("The embed code modal should display")
      eventually {
        cssSelector("#embed-code-modal").findElement should not be (None)
      }

      When("I click the close button on the embed code modal")
      click on cssSelector("#modal-dismiss")

      Then("I am taken back to the manage tabs screen")
      eventually {
        currentUrl should be (manageTabsUrl)
      }

      And("my new tab is displayed on the tabs list")
      val tabNames: List[String] =
        cssSelector(".tab-list .tab-name").findAllElements.map(_.text).toList
      tabNames should contain (tabName)
    }

    scenario("User edits a tab") {
      Given("I am on the manage tabs screen")
      go to (manageTabsUrl)

      When("I click the edit button for a tab")
      val tabId = cssSelector(".subscriber").element.attribute("data-tab-id") getOrElse ""
      val tabName = cssSelector(".tab-name").element.text
      click on cssSelector(".edit-tab")

      Then("I should be taken to the edit form for the tab")
      eventually {
        currentUrl should be (editTabUrl(tabId))
      }

      And("the name field should match the name of the tab")
      textField(cssSelector("#tab-name")).value should be (tabName)

      When("I change the name of the tab")
      val newTabName = randomString(20)
      textField(cssSelector("#tab-name")).value = newTabName

      And("I save my changes")
      click on cssSelector(".submit")

      Then("I should be redirected back to the manage tabs screen")
      eventually {
        currentUrl should be (manageTabsUrl)
      }

      And("the new tab name should be in the tabs list in place of the old name")
      cssSelector(".tab-name").element.text should be (newTabName)
    }

    scenario("User retrieves embed code for a tab") {
      Given("I am on the manage tabs screen")
      go to (manageTabsUrl)

      When("I click the embed code button")
      val tabId = cssSelector(".subscriber").element.attribute("data-tab-id") getOrElse ""
      click on cssSelector(".get-code")

      Then("I should be presented with a modal that has the embed code for that tab")
      eventually {
        cssSelector("#embed-code-modal").findElement should not be (None)
      }

      And("the embed code should contain the tab id somewhere in the code")
      textArea(cssSelector("#embed-code-modal textarea")).value should include regex(tabId)
    }

    scenario("User deletes a tab") {
      Given("I am on the manage tabs screen")
      go to (manageTabsUrl)

      When("I delete a tab")
      val tabId = cssSelector(".subscriber").element.attribute("data-tab-id") getOrElse ""
      click on cssSelector(".delete-tab")

      Then("the tab should disappear from the tabs list")
      eventually {
        cssSelector(".subscriber").findElement should be (None)
      }
    }
  }

  feature("Profile Management") {
    scenario("User views their profile") {
      Given("I am on the tab manager dashboard")
      go to (managerUrl)

      When("I click 'Profile' in the navigation")
      click on cssSelector("nav .profile a")

      Then("I should be directed to my profile form")
      eventually {
        currentUrl should be (profileUrl)
      }

      And("the 'Profile' item in the navigation should be highlighted")
      cssSelector("nav .profile").element.attribute("class") should be (Some("profile selected"))

      And("my email address should appear in the email field")
      val currentUserEmail = cssSelector("header .user-info span").element.text
      textField(cssSelector(".email")).value should be (currentUserEmail)
    }

    scenario("User updates profile information") {
      Given("I am on the profile form")
      go to (profileUrl)

      When("I update my profile information")
      val newFirstName = randomString(32)
      val newLastName = randomString(32)
      val newOrganization = randomString(32)

      textField(cssSelector(".first-name")).value = newFirstName
      textField(cssSelector(".last-name")).value = newLastName
      textField(cssSelector(".organization")).value = newOrganization

      And("click save")
      click on cssSelector(".submit")

      And("my new profile information should be reflected when I reload the page")
      // Give the reload time to happen.
      Thread.sleep(100)
      textField(cssSelector(".first-name")).value should be (newFirstName)
      textField(cssSelector(".last-name")).value should be (newLastName)
      textField(cssSelector(".organization")).value should be (newOrganization)
    }

    scenario("User attempts to remove email from profile") {
      Given("I am on the profile form")
      go to (profileUrl)

      When("I attempt to remove my email from the profile")
      textField(cssSelector(".email")).value = ""
      click on cssSelector(".submit")

      Then("I get a validation error")
      eventually {
        cssSelector(".email").element.attribute("class") should be (Some("email error"))
        cssSelector(".email + .validation-error").element.text should be ("Email is a required field.")
      }
    }

    scenario("User attempts to change their password, but doesn't match confirmation") {
      Given("I am on the profile form")
      go to (profileUrl)

      When("I mismatch values while attempting to change my password")
      click on cssSelector(".change-password")
      pressKeys(randomString(32))
      click on cssSelector(".confirm-password")
      pressKeys(randomString(32))

      And("click submit")
      click on cssSelector(".submit")

      Then("I should get a validation error")
      eventually {
        cssSelector(".change-password").element.attribute("class") should be (Some("change-password error"))
        cssSelector(".confirm-password").element.attribute("class") should be (Some("confirm-password error"))
        cssSelector(".confirm-password + .validation-error").element.text should be ("The passwords you selected do not match.")
      }
    }
  }

  feature("Connected Services Management") {
    scenario("User navigates to connected services page") {
      Given("I am on the tab manager dashboard")
      go to (managerUrl)

      When("I click the 'Connected Services' link")
      click on cssSelector("nav .connected-services > a")

      Then("I should be taken to the Connected Services page")
      eventually {
        currentUrl should be (connectedServicesUrl)
      }

      And("The 'Connected Services' item in the navigation should be highlighted")
      cssSelector("nav .connected-services").element.attribute("class") should be (Some("connected-services selected"))
    }

    scenario("User connects to Constant Contact") {
      val constantContactTestUsername = "hello@anchortab.com"
      val constantContactTestPassword = "AncH0r"

      Given("I am on the connected serivces page")
      go to (connectedServicesUrl)

      And("I don't have a connected constant contact account")
      cssSelector(".constant-contact-connection .connection-status").element.text should be ("Not connected.")

      When("I click the connect button")
      click on cssSelector(".constant-contact-connection .connect-service")

      Then("I should be directed to a Constant Contact login page")
      eventually {
        currentUrl should startWith ("https://login.constantcontact.com/login/?goto")
      }

      When("I enter my Constant Contact login credentials")
      click on cssSelector("#luser")
      pressKeys(constantContactTestUsername)
      click on "lpass"
      pressKeys(constantContactTestPassword)

      And("click login")
      click on "_save"

      Then("I should be presented with the Constant Contact grant access page")
      eventually {
        currentUrl should be ("https://oauth2.constantcontact.com/oauth2/oauth/confirm_access")
      }

      When("I click grant access")
      click on cssSelector("#authorize")

      Then("I should be back on the Connected Services page")
      eventually {
        currentUrl should be (connectedServicesUrl)
      }

      And("my Constant Contact username should appear under Constant Contact")
      cssSelector(".constant-contact-connection .connection-status").element.text should be ("Connected to " + constantContactTestUsername + ".")

      And("the disconnect button should be visible")
      cssSelector(".constant-contact-connection .disconnect-service").findElement should not be (None)
    }

    scenario("User connects to MailChimp") {
      val mailChimpTestUsername = "anchortabseleniumtests"
      val mailChimpTestPassword = "AncH0r"

      Given("I am on the connected serivces page")
      go to (connectedServicesUrl)

      And("I don't have a connected MailChimp account")
      cssSelector(".mailchimp-connection .connection-status").element.text should be ("Not connected.")

      When("I click the connect button")
      click on cssSelector(".mailchimp-connection .connect-service")

      Then("I should be directed to the MailChimp login page")
      eventually {
        currentUrl should startWith ("https://login.mailchimp.com/oauth2/authorize")
      }

      When("I enter my MailChimp login credentials")
      click on "username"
      pressKeys(mailChimpTestUsername)
      click on "password"
      pressKeys(mailChimpTestPassword)

      And("click login")
      click on cssSelector("fieldset > input")

      Then("I should be redirected back to the Connected Services page")
      eventually {
        currentUrl should be (connectedServicesUrl)
      }

      And("my MailChimp username should appear under MailChimp")
      cssSelector(".mailchimp-connection .connection-status").element.text should be ("Connected to " + mailChimpTestUsername + ".")

      And("the disconnect button should be visible")
      cssSelector(".mailchimp-connection .disconnect-service").findElement should not be (None)
    }
  }
}
