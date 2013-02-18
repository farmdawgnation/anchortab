package com.anchortab.acceptance

import org.scalatest._
import org.scalatest.selenium._

import net.liftweb.util.Helpers._

class RemoteManagerSpec(remoteUrl: String) extends PublicSpecImpl {
  protected val host = remoteUrl
}

class ManagerSpec extends AnchorTabSpec with ManagerSpecImpl
trait ManagerSpecImpl extends AcceptanceSpec {
  protected def host: String

  val managerUrl = host + "/manager"

  val manageTabsUrl = managerUrl + "/tabs"
  val newTabsUrl = managerUrl + "/tabs/new"
  def editTabUrl(tabId: String) = managerUrl + "/tab/" + tabId + "/edit"

  override def beforeAll() {
    super.beforeAll()

    val (email, password) = validUser

    go to (host)
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
      currentUrl should be (manageTabsUrl)

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

      When("I click 'Profile' in the navigation")

      Then("I should be directed to my profile form")

      And("the 'Profile' item in the navigation should be highlighted")

      And("my email address should appear in the email field")
      pending
    }

    scenario("User updates profile information") {
      Given("I am on the profile form")

      When("I update my profile information")

      And("click save")

      Then("a profile updated message should appear")

      And("my new profile information should be reflected when I reload the page")
      pending
    }

    scenario("User attempts to remove email from profile") {
      Given("I am on the profile form")

      When("I attempt to remove my email from the profile")

      Then("I get a validation error")
      pending
    }

    scenario("User changes their password") {
      Given("I am on the profile form")

      And("I enter a new value for my password")

      Then("I should get a profile updated message")
      pending
    }

    scenario("User attempts to change their password, but doesn't match confirmation") {
      Given("I am on the profile form")

      And("I mismatch values while attempting to change my password")

      Then("I should get a validation error")
      pending
    }
  }

  feature("Connected Services Management") {
    scenario("User navigates to connected services page") {
      Given("I am on the tab manager dashboard")

      When("I click the 'Connected Services' link")

      Then("I should be taken to the Connected Services page")

      And("The 'Connected Services' item in the navigation should be highlighted")
      pending
    }

    scenario("User connects to Constant Contact") {
      Given("I am on the connected serivces page")

      And("I don't have a connected constant contact account")

      When("I click the connect button")

      And("enter my Constant Contact login credentials")

      And("click login")

      Then("I should be back on the Connected Services page")

      And("my Constant Contact username should appear under Constant Contact")

      And("the disconnect button should be visible")
      pending
    }

    scenario("User connects to MailChimp") {
      Given("I am on the connected serivces page")

      And("I don't have a connected MailChimp account")

      When("I click the connect button")

      And("enter my MailChimp login credentials")

      And("click login")

      Then("I should be back on the Connected Services page")

      And("my MailChimp username should appear under MailChimp")

      And("the disconnect button should be visible")
      pending
    }
  }
}
