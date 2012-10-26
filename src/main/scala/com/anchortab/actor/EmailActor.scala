package com.anchortab.actor

import net.liftweb._
  import common._
  import actor._
  import mongodb._
  import util.Helpers._
  import json._
    import ext._
    import JsonDSL._
    import Extraction._

import org.bson.types.ObjectId

object EmailActor extends LiftActor {
  def messageHandler = {
    case _ =>
  }
}
