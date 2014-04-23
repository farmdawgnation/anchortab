package com.anchortab.snippet

import net.liftweb._
  import common._
  import http._
  import sitemap._
    import Loc._

import com.anchortab.model._

object EmailUsers {
  val menu = Menu.i("Email Users") / "admin" / "email-users" >>
    If(
      () => userSession.is.map(_.userId).flatMap(id => User.find(id)).map(_.admin_?).getOrElse(false),
      () => NotFoundResponse()
    )
}
