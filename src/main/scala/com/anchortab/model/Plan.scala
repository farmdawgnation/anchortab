package com.anchortab.model

import net.liftweb._
  import mongodb._

import org.joda.DateTime

import org.bson.types.ObjectId

case class Plan(name:String, description:String, price:Double,
                features:HashMap[String,Int], starts:DateTime,
                ends:Option[DateTime] = None, _id:ObjectId = ObjectId.get) extends MongoDocument[Plan] {
  val meta = Plan
}
object Plan extends MongoDocumentMeta[Plan]

case class PlanSubscription(planId:ObjectId, userId:ObjectId, price:Double, preapprovalId:Long,
                            createdAt:DateTime = new DateTime, cancelled:Boolean = false,
                            ends:Option[DateTime] = None, lastRenewal:Option[DateTime] = None,
                            _id:ObjectId = ObjectId.get) extends MongoDocument[PlanSubscription] {
  val meta = PlanSubscription
}
object PlanSubscription extends MongoDocumentMeta[PlanSubscription]
