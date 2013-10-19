package com.anchortab.model

import scala.collection.immutable.HashMap

import net.liftweb._
  import common._
  import mongodb._
    import BsonDSL._
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
                            createdAt:DateTime = new DateTime, status:String = "trial",
                            begins:DateTime = new DateTime, ends:Option[DateTime] = None,
                            lastBilled:Option[DateTime] = None,
                            _id:ObjectId = ObjectId.get) {
  lazy val trial_? = status == "trial"
  lazy val active_? = status == "active"
  lazy val cancelled_? = status == "cancelled"
  lazy val stopped_? = status == "stopped"

  // A valid subscription is any subscription that allows your tabs to display
  // on your site.
  lazy val valid_? = {
    if (trial_? || active_?)
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

case class UserServiceCredentials(serviceName:String, userIdentifier:String, serviceCredentials:Map[String, String])

case class UserFirstStep(description: String, href: String)
object UserFirstStep {
  object Keys {
    val ConnectAnExternalService = "connect-an-external-serivce"
    val CreateATab = "create-a-tab"
    val EmbedYourTab = "embed-your-tab"
  }

  object Steps {
    val ConnectAnExternalService = UserFirstStep("Connect to a Campaign Monitor, Constant Contact, or MailChimp account.", "/manager/services")
    val CreateATab = UserFirstStep("Create your first Anchor Tab.", "/manager/tabs/new")
    val EmbedYourTab = UserFirstStep("Embed your Anchor Tab on your site.", "/manager/tabs")
  }
}

case class UserPasswordResetKey(key: String = randomString(32),
                                expires: DateTime = (new DateTime()).plusHours(24))

case class UserActiveCard(last4: String, cardType: String, expMonth: Int, expYear: Int)

/**
 * User model. This class represnts a distinct user on the system.
**/
case class User(email:String, password:String, profile:Option[UserProfile] = None,
                authorizations:List[UserAuthorizationKey] = List(),
                subscriptions:List[UserSubscription] = List(),
                invoices:List[UserInvoice] = List(),
                serviceCredentials:List[UserServiceCredentials] = List(),
                quotaCounts:Map[String, Long] = Map.empty,
                quotasLastReset:Option[DateTime] = None,
                firstSteps: Map[String, UserFirstStep] = Map.empty,
                passwordResetKey: Option[UserPasswordResetKey] = None,
                role:Option[String] = None, createdAt:DateTime = new DateTime,
                stripeCustomerId:Option[String] = None,
                activeCard:Option[UserActiveCard] = None,
                referringAffiliateId: Option[ObjectId] = None,
                affiliateCode: Option[String] = None,
                _id:ObjectId = ObjectId.get) extends MongoDocument[User] {
  val meta = User

  lazy val subscription = subscriptions.filter(_.valid_?).lastOption
  lazy val validSubscription_? = subscription.isDefined

  lazy val plan = subscription.flatMap(_.plan) getOrElse Plan.DefaultPlan
  lazy val onSpecialPlan_? = plan.isSpecial

  lazy val admin_? = role == Some(User.Roles.Admin)
  lazy val affiliate_? = affiliateCode.isDefined

  lazy val name = {
    val firstName =
      {
        for {
          profile <- profile
          firstName <- profile.firstName
        } yield {
          firstName
        }
      } getOrElse {
        ""
      }

    val lastName =
      {
        for {
          profile <- profile
          lastName <- profile.lastName
        } yield {
          lastName
        }
      } getOrElse {
        ""
      }

    firstName + " " + lastName
  }

  def credentialsFor(serviceName:String) = {
    serviceCredentials.filter(_.serviceName == serviceName).headOption
  }

  def nearQuotaFor_?(quotaedEvent:String) = {
    {
      for {
        quotaLimit <- plan.quotas.get(quotaedEvent)
        currentUsage <- quotaCounts.get(quotaedEvent)
      } yield {
        currentUsage > (quotaLimit * 0.75)
      }
    }.foldLeft(false)(_ && _)
  }

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

  lazy val tabsActive_? = admin_? || (
    subscription.isDefined &&
    withinQuotaFor_?(Plan.Quotas.EmailSubscriptions) &&
    withinQuotaFor_?(Plan.Quotas.Views) &&
    plan.quotas.get(Plan.Quotas.NumberOfTabs).map(_ >= Tab.count("userId" -> _id)).getOrElse(true)
  )

  lazy val asJson = {
    implicit val formats = DefaultFormats

    ("id" -> _id.toString) ~
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
      user <- User.find("email" -> (
        ("$regex" -> ("^" + email + "$")) ~
        ("$options" -> "i")
      )):Box[User]
      passwordMatches = User.checkPassword(password, user.password)
        if passwordMatches
    } yield {
      user
    }
  }
}
