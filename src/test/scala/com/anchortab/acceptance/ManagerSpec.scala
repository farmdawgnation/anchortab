package com.anchortab.acceptance

import org.scalatest._
import org.scalatest.selenium._

trait ManagerSpec extends AcceptanceSpec {
  protected def host: String

  info("")
  info("As a registered user")
  info("I want to be able to use the Tab Manager")
  info("So I can manager by tabs and my account.")
  info("")

  feature("Tab Management") {
    scenario("User views /manager/tabs with no tabs on account") (pending)

    scenario("User views /manager/tabs with one unviewed tab") (pending)

    scenario("User creates a tab") (pending)

    scenario("User edits a tab") (pending)

    scenario("User deletes a tab") (pending)

    scenario("User retrieves embed code for a tab") (pending)
  }

  feature("Profile Management") {
    scenario("User views their profile") (pending)

    scenario("User updates profile information") (pending)

    scenario("User attempts to remove email from profile") (pending)

    scenario("User changes their password") (pending)

    scenario("User attempts to change their password, but doesn't match confirmation") (pending)
  }

  feature("Connected Services Management") {
    scenario("User does not have a connected Constant Contact account") (pending)

    scenario("User does have a connected Constant Contact account") (pending)

    scenario("User does not have a connected MailChimp account") (pending)

    scenario("User does have a connected MailChimp account") (pending)
  }
}
