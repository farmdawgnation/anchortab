package com.anchortab.snippet

import net.liftweb._
  import common._
  import sitemap._
    import Loc._
  import http._
    import SHtml._
  import util._
    import Helpers._
  import js._
    import JsCmds._

object ForgotPasswordComplete {
  val menu = Menu.i("Reset Email Sent") / "amnesia-complete" >>
    Authentication.ifNotLoggedIn
}

