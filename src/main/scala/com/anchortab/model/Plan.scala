package com.anchortab.model

import scala.collection.immutable.HashMap

import net.liftweb._
  import mongodb._
  import json.ext._

import org.joda.time.DateTime

import org.bson.types.ObjectId

case class PlanTerm(description:String, abbreveation:String, stripeCode:String)

/**
 * Model for a Plan that users can subscribe to on Anchor Tab.
 *
 * Special note: the quotas are all per month values regardless of
 * the term of the plan.
**/
case class Plan(name:String,
                description:String,
                price:Double,
                trialDays:Int,
                features:Map[String,Boolean],
                quotas:Map[String, Long],
                isSpecial:Boolean = false,
                visibleOnRegistration:Boolean = true,
                starts:Option[DateTime] = None,
                ends:Option[DateTime] = None,
                term:PlanTerm = Plan.MonthlyTerm,
                stripeId: Option[String] = None,
                _id:ObjectId = ObjectId.get
    ) extends MongoDocument[Plan] {
  val meta = Plan

  val formattedPrice = "$" + ("%1.2f" format price)
  val free_? = price == 0
  val hasTrial_? = trialDays > 0

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

  val MonthlyTerm = PlanTerm("monthly", "mo", "month")
  val YearlyTerm = PlanTerm("yearly", "yr", "year")

  val terms = MonthlyTerm :: YearlyTerm :: Nil

  object Quotas {
    val NumberOfTabs = "number-of-tabs"
    val EmailSubscriptions = "email-subscriptions"
    val Views = "views"

    def humanNameFor(quotaName: String) = {
      quotaName match {
        case NumberOfTabs => "Number of Tabs"
        case EmailSubscriptions => "Email Subscriptions"
        case Views => "Views"
        case _ => ""
      }
    }
  }

  object Features {
    val BasicAnalytics = "basic-analytics"
    val WhitelabeledTabs = "whitelabeled-tabs"
    val CustomColorSchemes = "custom-color-schemes"
    val ApiAccess = "api-access"
  }

  // The DefaultPlan, or the plan you're on if you don't have a plan. If we decide to offer
  // a free tier at some point in the future, we should change this plan to describe the
  // free tier.
  val DefaultPlan = Plan("Not subscribed.", "You are currently not subscribed to a plan.", 0, 0,
                         Map.empty, Map(Quotas.EmailSubscriptions -> 0, Quotas.Views -> 0))
}
