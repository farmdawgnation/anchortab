package com.anchortab.snippet

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
    case "jquery" :: Nil => jquery
  }

  def jquery =
    "script" #> <script src="/javascripts/jquery-1.8.2.js" type="text/javascript"></script>
}