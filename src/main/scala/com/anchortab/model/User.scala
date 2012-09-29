package com.anchortab.model

import scala.collection.immutable.HashMap

import net.liftweb._
  import mongodb._
  import util.Helpers._

import org.joda.time.DateTime

import org.bson.types.ObjectId

/**
 * Model representing a user session. A user session is sored in a
 * Lift SessionVar to associate the current Lift session to a logged
 * in user.
**/
case class UserSession(userId:ObjectId, ip:String, userAgent:String,
                       createdAt:DateTime = new DateTime,
                       _id: ObjectId = ObjectId.get) extends MongoDocument[UserSession] {
  val meta = UserSession
}
object UserSession extends MongoDocumentMeta[UserSession] {
}

// User model helper classes
case class UserAuthorizationKey(appName:String, key:String = randomString(32),
                                createdAt:DateTime = new DateTime)
case class UserProfile(firstName:Option[String], lastName:Option[String],
                       organization:Option[String])
case class UserSubscription(planId:ObjectId, price:Double, preapprovalId:Long, term:String,
                            createdAt:DateTime = new DateTime, status:String = "new",
                            begins:Option[DateTime] = None, ends:Option[DateTime] = None,
                            _id:ObjectId = ObjectId.get) {
  lazy val new_? = status == "new"
  lazy val active_? = status == "active"
  lazy val cancelled_? = status == "cancelled"
  lazy val stopped_? = status == "stopped"

  // A valid subscription is any subscription that allows your tabs to display
  // on your site.
  lazy val valid_? = {
    if (active_?)
      begins.map(_ isBeforeNow).getOrElse(true) && ends.map(_ isAfterNow).getOrElse(true)
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
                quotaCounts:HashMap[String, Long] = HashMap.empty,
                role:Option[String] = None, createdAt:DateTime = new DateTime,
                _id:ObjectId = ObjectId.get) extends MongoDocument[User] {
  val meta = User

  lazy val subscription = subscriptions.filter(_.valid_?).lastOption
  lazy val validSubscription_? = subscription.isDefined

  lazy val admin_? = role == Some(User.Roles.Admin)

  lazy val withinQuota_? = {
    {
      for {
        subscription <- subscription.toSeq
        plan <- subscription.plan.toSeq
        quotaedEvent <- plan.quotas.keys
        quotaLimit <- plan.quotas.get(quotaedEvent)
        currentUsage <- quotaCounts.get(quotaedEvent)
      } yield {
        currentUsage < quotaLimit
      }
    }.foldLeft(true)(_ && _)
  }

  lazy val tabsActive_? = admin_? || (validSubscription_? && withinQuota_?)
}
object User extends MongoDocumentMeta[User] {
  object Roles {
    val Admin = "admin"
  }
}