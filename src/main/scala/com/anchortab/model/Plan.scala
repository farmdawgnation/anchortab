package com.anchortab.model

import scala.collection.immutable.HashMap

import net.liftweb._
  import mongodb._

import org.joda.time.DateTime

import org.bson.types.ObjectId

case class Plan(name:String, description:String, price:Double,
                features:HashMap[String,Boolean], quotas:HashMap[String, Long],
                starts:DateTime, ends:Option[DateTime] = None, term:String = "monthly",
                _id:ObjectId = ObjectId.get) extends MongoDocument[Plan] {
  val meta = Plan

  def hasFeature_?(feature:String) = {
    features.get(feature) match {
      case Some(true) => true
      case _ => false
    }
  }
  def quotaFor(quotaKey:String) = quotas.get(quotaKey)
}
object Plan extends MongoDocumentMeta[Plan] {
  object Quotas {
    val EmailSubscriptions = "email-subscriptions"
    val Views = "views"
  }

  object Features {
    val ManageTabs = "manage-tabs"
    val BasicAnalytics = "basic-analytics"
  }
}
