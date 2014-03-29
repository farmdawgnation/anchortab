package com.anchortab.model

import net.liftweb._
  import common._
  import mongodb._
    import BsonDSL._
  import util.Helpers._
  import json._
    import Extraction._

import com.anchortab.campaignmonitor._
import com.anchortab.actor._
import com.anchortab.util._
import com.anchortab.model._

import org.joda.time._

import com.ecwid.mailchimp._
  import method.list._

import org.bson.types.ObjectId

case class ServiceIFrameParameters(
  uri: String,
  emailFieldName: String,
  firstNameFieldName: String
)

/**
 * The ServiceWrapper trait is the common interface for all wrappers around external email
 * subscription services. The only two functions these wrappers are required to expose are
 * a subscribe and an unsubscribe. The mechanics and individual settings of how they handle
 * those operations are irrelevent to the parent interface.
**/
sealed trait ServiceWrapper {
  def credentialsValid_? : Boolean = true
  def iFrameParameters: Option[ServiceIFrameParameters] = None

  def subscribeEmail(email: String, name: Option[String] = None) : Box[Boolean]
  def unsubscribeEmail(email:String) : Box[Boolean]

  def wrapperIdentifier: String
}
object ServiceWrapper {
  val typeHints = ShortTypeHints(List(
    classOf[MailChimpServiceWrapper],
    classOf[ConstantContactServiceWrapper],
    classOf[CampaignMonitorServiceWrapper],
    classOf[LeadGenerationServiceWrapper],
    classOf[PardotServiceWrapper],
    classOf[AlwaysSuccessfulServiceWrapper],
    classOf[AlwaysFailureServiceWrapper],
    classOf[AlwaysEmptyServiceWrapper]
  ))
}

case class AlwaysSuccessfulServiceWrapper() extends ServiceWrapper {
  override val wrapperIdentifier = "Always Successful"
  override val iFrameParameters = None
  def subscribeEmail(email: String, name: Option[String]) = Full(true)
  def unsubscribeEmail(email: String) = Full(true)
}
case class AlwaysFailureServiceWrapper() extends ServiceWrapper {
  override val wrapperIdentifier = "Always failure"
  override val iFrameParameters = None
  def subscribeEmail(email: String, name: Option[String]) = Failure("Chablewit.")
  def unsubscribeEmail(email: String) = Failure("Chablewit.")
}
case class AlwaysEmptyServiceWrapper() extends ServiceWrapper {
  override val wrapperIdentifier = "Always empty"
  override val iFrameParameters = None
  def subscribeEmail(email: String, name: Option[String]) = Empty
  def unsubscribeEmail(email: String) = Empty
}

case class PardotServiceWrapper(userId: ObjectId, targetUri: String, emailFieldName: String, firstNameFieldName: String) extends ServiceWrapper {
  override val wrapperIdentifier = "Pardot - " + userId.toString + " - " + targetUri

  override val iFrameParameters = {
    for {
      user <- User.find(userId)
    } yield {
      ServiceIFrameParameters(
        targetUri,
        emailFieldName,
        firstNameFieldName
      )
    }
  }

  def subscribeEmail(email: String, name: Option[String] = None) = Full(true)
  def unsubscribeEmail(email: String) = Full(true)
}

case class LeadGenerationServiceWrapper(targetEmail: String) extends ServiceWrapper {
  override val credentialsValid_? = true
  val wrapperIdentifier = "I don't show up on services screen."

  def subscribeEmail(email: String, name: Option[String] = None) = {
    EmailActor ! SendLeadGenerationSubscriptionEmail(targetEmail, email, name)
    Full(true)
  }

  def unsubscribeEmail(email: String) = {
    // TODO
    Full(true)
  }
}

case class CampaignMonitorServiceWrapper(userId: ObjectId, listId: String) extends ServiceWrapper
                                                                              with CampaignMonitorCredentialsHelper
                                                                              with Loggable {
  override val credentialsValid_? = true

  def subscribeEmail(email: String, name: Option[String] = None) = {
    val result = withAccessCredentials(userId) { (accessToken, refreshToken) =>
      CampaignMonitor.addSubscriber(accessToken, refreshToken, listId, email, name)
    }

    result.map(thing => true)
  }

  def unsubscribeEmail(email: String) = {
    val result = withAccessCredentials(userId) { (accessToken, refreshToken) =>
      CampaignMonitor.removeSubscriber(accessToken, refreshToken, listId, email)
    }

    result.map(thing => true)
  }

  def wrapperIdentifier = {
    "Campaign Monitor - " + userId.toString
  }
}

case class ConstantContactServiceWrapper(username:String, implicit val accessToken:String, listId:Long) extends ServiceWrapper with Loggable {
  import com.anchortab.constantcontact.model.Contacts._
  import com.anchortab.constantcontact.model.ContactLists._

  // This is an OAuth-based API wrapper, making the checking of valid credentials unneeded for the moment
  override val credentialsValid_? = true

  def subscribeEmail(email:String, name: Option[String] = None) = {
    val contact = Contact(
      EmailAddress(email) :: Nil,
      name = name.map(name => ContactName(Some(name), None, None)),
      lists = Some(ContactList(listId.toString) :: Nil)
    )

    for {
      contact <- contact.save
    } yield {
      true
    }
  }

  def unsubscribeEmail(email:String) = {
    for {
      contact <- Contact.find(email)
      deleteResult <- contact.delete
    } yield {
      true
    }
  }

  def wrapperIdentifier = {
    "Constant Contact - " + username
  }
}

case class MailChimpServiceWrapper(userId:ObjectId, listId:String) extends ServiceWrapper with Loggable {
  protected def retrieveApiKey = {
    for {
      user <- (User.find(userId): Box[User])
      credentials <- (user.credentialsFor("Mailchimp"): Box[UserServiceCredentials]) ?~! "No credentials for Mailchimp"
      token <- (credentials.serviceCredentials.get("token"): Box[String]) ?~! "No token in MailChimp credentials."
    } yield {
      token
    }
  }

  override def credentialsValid_? = true

  protected def buildSubscribeCall(apiKey: String, email: String, name: Option[String]) = {
    // Build the subscribe request
    val listSubscribeCall = new ListSubscribeMethod
    listSubscribeCall.apikey = apiKey
    listSubscribeCall.id = listId
    listSubscribeCall.email_address = email
    listSubscribeCall.update_existing = true

    name.foreach { name =>
      listSubscribeCall.merge_vars = new MergeVars(name)
    }

    listSubscribeCall
  }

  def subscribeEmail(email:String, name: Option[String] = None) = {
    // Instantiate a MailChimpClient
    val mcClient = new MailChimpClient

    for {
      apiKey <- retrieveApiKey
      listSubscribeCall = buildSubscribeCall(apiKey, email, name)
      subscribeResult <- tryo(mcClient.execute(listSubscribeCall))
    } yield {
      subscribeResult
    }
  }

  protected def buildUnsubscribeCall(apiKey: String, email: String) = {
    // Build the unsubscribe request
    val listUnsubscribeCall = new ListUnsubscribeMethod
    listUnsubscribeCall.apikey = apiKey
    listUnsubscribeCall.id = listId
    listUnsubscribeCall.email_address = email
    listUnsubscribeCall
  }

  def unsubscribeEmail(email:String) = {
    // Instantiate a MailChimpClient
    val mcClient = new MailChimpClient

    for {
      apiKey <- retrieveApiKey
      listUnsubscribeCall = buildUnsubscribeCall(apiKey, email)
      unsubscribeResult <- tryo(mcClient.execute(listUnsubscribeCall))
    } yield {
      unsubscribeResult
    }
  }

  def wrapperIdentifier = {
    "MailChimp - " + listId
  }
}
