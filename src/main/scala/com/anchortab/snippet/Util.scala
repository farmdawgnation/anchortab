package com.anchortab.snippet

import scala.xml._

import net.liftweb._
  import common._
  import http._
    import LiftRules._
    import rest._
    import js._
      import JE._
      import JsExp._
  import util._
    import Helpers._
  import json._
    import JsonDSL._
    import Extraction._
  import mongodb._
    import BsonDSL._

object Util extends Loggable {
  def snippetHandlers : SnippetPF = {
    case "ie-conditional" :: Nil => ieConditional _
    case "jquery" :: Nil => jquery _
  }

  def ieConditional(xhtml:NodeSeq) = {
    val ieVersion = S.attr("version") openOr "IE"
    Unparsed("<!--[if " + ieVersion + "]>") ++ xhtml ++ Unparsed("<![endif]-->")
  }

  def jquery(xhtml:NodeSeq) = {
    val jQueryUrl = Props.get("jquery.url")

    jQueryUrl.map { url =>
      ("script [src]" #> url).apply(xhtml)
    } openOr {
      xhtml
    }
  }
}
