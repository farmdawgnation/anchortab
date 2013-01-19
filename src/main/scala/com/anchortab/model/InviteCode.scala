package com.anchortab.model

import scala.collection.immutable.HashMap

import net.liftweb._
  import common._
  import mongodb._
  import util.Helpers._
  import json._
    import ext._
    import JsonDSL._
    import Extraction._
  import util._
    import Helpers._

import org.joda.time.DateTime

import org.bson.types.ObjectId

case class InviteCode(numberOfUses: Int = 0, numberOfUsesAvailable: Option[Int] = None,
                      validityStart: Option[DateTime] = None, validityEnd: Option[DateTime] = None,
                      forPlanId: Option[ObjectId] = None, code: String = randomString(32),
                      _id:ObjectId = ObjectId.get) extends MongoDocument[InviteCode] {

  val forPlan = forPlanId.flatMap(Plan.find(_))
  val meta = InviteCode

  val url = {
    Props.mode match {
      case Props.RunModes.Production =>
        "http://anchortab.com/accept-invite/" + code

      case Props.RunModes.Staging =>
        "http://staging.anchortab.com/accept-invite/" + code

      case Props.RunModes.Development =>
        "http://local.anchortab.com/accept-invite/" + code
    }
  }
}
object InviteCode extends MongoDocumentMeta[InviteCode] {
  override def formats = allFormats ++ JodaTimeSerializers.all
}
