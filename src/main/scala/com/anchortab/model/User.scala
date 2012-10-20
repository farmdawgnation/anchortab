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

import org.joda.time.DateTime

import org.bson.types.ObjectId

import org.mindrot.jbcrypt.BCrypt

/**
 * Model representing a user session. A user session is sored in a
 * Lift SessionVar to associate the current Lift session to a logged
 * in user.
**/
case class UserSession(userId:ObjectId, ip:String, userAgent:String,
                       createdAt:DateTime = new DateTime,
                       _id: ObjectId = ObjectId.get) extends MongoDocument[UserSession] {
  val meta = UserSession

  lazy val user = User.find(userId)
}
object UserSession extends MongoDocumentMeta[UserSession] {
  override def formats = allFormats ++ JodaTimeSerializers.all
}

// User model helper classes
case class UserAuthorizationKey(appName:String, key:String = randomString(32),
                                createdAt:DateTime = new DateTime)
case class UserProfile(firstName:Option[String], lastName:Option[String],
                       organization:Option[String])
case class UserSubscription(planId:ObjectId, price:Double, term:PlanTerm,
                            preapprovalId:Option[Long] = None, preapprovalUri:Option[String] = None,
                            createdAt:DateTime = new DateTime, status:String = "new",
                            begins:DateTime = new DateTime, ends:Option[DateTime] = None,
                            _id:ObjectId = ObjectId.get) {
  lazy val new_? = status == "new"
  lazy val active_? = status == "active"
  lazy val cancelled_? = status == "cancelled"
  lazy val stopped_? = status == "stopped"

  // A valid subscription is any subscription that allows your tabs to display
  // on your site.
  lazy val valid_? = {
    if (active_?)
      begins.isBeforeNow && ends.map(_ isAfterNow).getOrElse(true)
    else if (cancelled_?)
      ends.map(_ isAfterNow).getOrElse(false)
    else
      false
  }

  lazy val plan = Plan.find(planId)
}
case class UserInvoice(subscriptionId:ObjectId, price:Double, status:String, date:DateTime,
                       _id:ObjectId = ObjectId.get)

/**
 * User model. This class represnts a distinct user on the system.
**/
case class User(email:String, password:String, profile:Option[UserProfile] = None,
                authorizations:List[UserAuthorizationKey] = List(),
                subscriptions:List[UserSubscription] = List(),
                invoices:List[UserInvoice] = List(),
                quotaCounts:Map[String, Long] = Map.empty,
                role:Option[String] = None, createdAt:DateTime = new DateTime,
                _id:ObjectId = ObjectId.get) extends MongoDocument[User] {
  val meta = User

  lazy val subscription = subscriptions.filter(_.valid_?).lastOption
  lazy val validSubscription_? = subscription.isDefined

  lazy val plan = subscription.flatMap(_.plan) getOrElse Plan.DefaultPlan

  lazy val admin_? = role == Some(User.Roles.Admin)

  def withinQuotaFor_?(quotaedEvent:String) = {
    {
      for {
        quotaLimit <- plan.quotas.get(quotaedEvent)
        currentUsage <- quotaCounts.get(quotaedEvent)
      } yield {
        currentUsage < quotaLimit
      }
    }.foldLeft(true)(_ && _)
  }

  lazy val tabsActive_? = admin_? || withinQuotaFor_?(Plan.Quotas.EmailSubscriptions)

  lazy val asJson = {
    implicit val formats = DefaultFormats

    ("email" -> email) ~
    ("profile" -> decompose(profile)) ~
    ("role" -> role) ~
    ("subscriptionValid" -> validSubscription_?) ~
    ("quotaCounts" -> quotaCounts) ~
    ("tabsActive" -> tabsActive_?)
  }
}
object User extends MongoDocumentMeta[User] {
  override def formats = allFormats ++ JodaTimeSerializers.all

  object Roles {
    val Admin = "admin"
  }

  def hashPassword(inputPassword:String) = {
    BCrypt.hashpw(inputPassword, BCrypt.gensalt())
  }

  def checkPassword(candidatePassword:String, hashedPassword:String) = {
    BCrypt.checkpw(candidatePassword, hashedPassword)
  }

  def attemptLogin(email:String, password:String) = {
    for {
      user <- User.find("email" -> email):Box[User]
      passwordMatches = User.checkPassword(password, user.password)
        if passwordMatches
    } yield {
      user
    }
  }
}
