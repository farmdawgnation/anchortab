package com.anchortab.model

import scala.collection.immutable.HashMap

import net.liftweb._
  import mongodb._

import org.joda.time.DateTime

import org.bson.types.ObjectId

/**
 * The Features singleton.
**/
object Features {
  object Admin {
    val ManageUsers = "manage-users"
  }
}

case class Plan(name:String, description:String, price:Double,
                features:HashMap[String,Boolean], starts:DateTime,
                ends:Option[DateTime] = None, term:String = "monthly",
                _id:ObjectId = ObjectId.get) extends MongoDocument[Plan] {
  val meta = Plan

  def hasFeature_?(feature:String) = {
    features.get(feature) match {
      case Some(true) => true
      case _ => false
    }
  }
}
object Plan extends MongoDocumentMeta[Plan]

case class PlanSubscription(planId:ObjectId, userId:ObjectId, price:Double, preapprovalId:Long,
                            term:String, createdAt:DateTime = new DateTime, cancelled:Boolean = false,
                            active:Boolean = false, paymentFailure:Boolean = false,
                            ends:Option[DateTime] = None, lastRenewal:Option[DateTime] = None,
                            _id:ObjectId = ObjectId.get) extends MongoDocument[PlanSubscription] {
  val meta = PlanSubscription

  lazy val nextRenewal = lastRenewal.map(calculateNextRenewal(_))
  lazy val user = User.find(userId)
  lazy val plan = Plan.find(planId)

  private def calculateNextRenewal(lastRenewal:DateTime) = {
    term match {
      case "monthly" =>
        lastRenewal.plusMonths(1)
      case "yearly" =>
        lastRenewal.plusYears(1)
    }
  }
}
object PlanSubscription extends MongoDocumentMeta[PlanSubscription]
