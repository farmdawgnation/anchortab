package com.anchortab.snippet

import scala.xml.NodeSeq

import net.liftweb._
  import common._
  import http._
    import js._
      import JsCmds._
    import LiftRules._
  import json._
    import JsonDSL._
  import util._
    import Helpers._

object Subscription extends Loggable {
  def snippetHandlers : SnippetPF = {
    case "subscription-summary" :: Nil => subscriptionSummary
    case "plan-selection" :: Nil => planSelection
    case "billing-summary" :: Nil => billingSummary
  }

  def subscriptionSummary = {
    PassThru
  }

  def planSelection = {
    PassThru
  }

  def billingSummary = {
    PassThru
  }
}
