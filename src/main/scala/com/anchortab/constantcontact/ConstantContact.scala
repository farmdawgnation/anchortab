package com.anchortab.constantcontact

import net.liftweb._
  import common._
  import json._

object ConstantContact {
  private val endpointBase = "api.constantcontact.com"
  private val endpointVersion = "v2"

  private[constantcontact] def get(resource:String, params:Map[String,String] = Map.empty) : Box[JValue] = {
    // TODO
    Failure("Not implemented.")
  }

  private[constantcontact] def post(resource:String, body:JValue) : Box[JValue] = {
    // TODO
    Failure("Not implemented.")
  }

  private[constantcontact] def put(resource:String, body:JValue) : Box[JValue] = {
    // TODO
    Failure("Not implemented.")
  }

  private[constantcontact] def delete(resource:String) : Box[JValue] = {
    // TODO
    Failure("Not implemented.")
  }
}
