package com.anchortab.model

import scala.collection.immutable.HashMap

import net.liftweb._
  import mongodb._
  import json.ext._

import org.joda.time.DateTime

import org.bson.types.ObjectId

case class PlanTerm(description:String, abbreveation:String)

/**
 * Model for a Plan that users can subscribe to on Anchor Tab.
 *
 * Special note: the quotas are all per month values regardless of
 * the term of the plan.
**/
case class Plan(name:String, description:String, price:Double,
                features:Map[String,Boolean], quotas:Map[String, Long],
                visibleOnRegistration:Boolean = true,
                starts:Option[DateTime] = None, ends:Option[DateTime] = None,
                term:PlanTerm = Plan.MonthlyTerm, _id:ObjectId = ObjectId.get
    ) extends MongoDocument[Plan] {
  val meta = Plan

  val formattedPrice = "$" + ("%1.2f" format price)
  val free_? = price == 0

  def hasFeature_?(feature:String) = {
    features.get(feature) match {
      case Some(true) => true
      case _ => false
    }
  }
  def quotaFor(quotaKey:String) = quotas.get(quotaKey)

  lazy val registrationTitle = {
    price match {
      case 0 =>
        name + " (Free)"

      case _ =>
        name + " (" + formattedPrice + "/" + term.abbreveation + ")"
    }
  }
}
object Plan extends MongoDocumentMeta[Plan] {
  override def formats = allFormats ++ JodaTimeSerializers.all

  val MonthlyTerm = PlanTerm("monthly", "mo")
  val YearlyTerm = PlanTerm("yearly", "yr")

  object Quotas {
    val NumberOfTabs = "number-of-tabs"
    val EmailSubscriptions = "email-subscriptions"
    val Views = "views"
  }

  object Features {
    val BasicAnalytics = "basic-analytics"
    val WhitelabeledTabs = "whitelabeled-tabs"
  }

  // The DefaultPlan, or the plan you're on if you don't have a plan. If we decide to offer
  // a free tier at some point in the future, we should change this plan to describe the
  // free tier.
  val DefaultPlan = Plan("Not subscribed.", "You are currently not subscribed to a plan.", 0,
                         Map.empty, Map(Quotas.EmailSubscriptions -> 0, Quotas.Views -> 0))
}
