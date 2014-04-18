package com.anchortab.snippet

import net.liftweb._
  import sitemap._
    import Loc._
  import common._
  import http._
    import SHtml._

object Invoice {
  val menu = Menu.param[String](
    "Invoice",
    "Invoice",
    s => Full(s),
    s => s
  ) / "manager" / "invoice" / * >>
  TemplateBox(() => Templates("manager" :: "invoice" :: Nil))
}
