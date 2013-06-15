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

import org.joda.time._

import com.ecwid.mailchimp._
  import method.list._

import org.bson.types.ObjectId

/**
 * The ServiceWrapper trait is the common interface for all wrappers around external email
 * subscription services. The only two functions these wrappers are required to expose are
 * a subscribe and an unsubscribe. The mechanics and individual settings of how they handle
 * those operations are irrelevent to the parent interface.
**/
sealed trait ServiceWrapper {
  def credentialsValid_? : Boolean
  def subscribeEmail(email: String, name: Option[String] = None) : Box[Boolean]
  def unsubscribeEmail(email:String) : Box[Boolean]

  def wrapperIdentifier: String
}
object ServiceWrapper {
  val typeHints = ShortTypeHints(List(
    classOf[MailChimpServiceWrapper],
    classOf[ConstantContactServiceWrapper],
    classOf[CampaignMonitorServiceWrapper],
    classOf[LeadGenerationServiceWrapper]
  ))
}

case class LeadGenerationServiceWrapper(targetEmail: String) extends ServiceWrapper {
  val credentialsValid_? = true
  val wrapperIdentifier = "I don't show up on services screen."

  def subscribeEmail(email: String, name: Option[String] = None) = {
    EmailActor ! SendLeadGenerationSubscriptionEmail(targetEmail, email)
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
  val credentialsValid_? = true

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
  val credentialsValid_? = true

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

case class MailChimpServiceWrapper(apiKey:String, listId:String) extends ServiceWrapper with Loggable {
  def credentialsValid_? = {
    // Instantiate a MailChimpClient
    val mcClient = new MailChimpClient

    // Build a dummy request
    val listMembersCall = new ListMembersMethod
    listMembersCall.apikey = apiKey
    listMembersCall.id = listId

    //Try to execute it
    tryo(mcClient.execute(listMembersCall)) match {
      case Full(_:ListMembersResult) => true
      case Failure(msg, _, _) =>
        logger.error("MailChimp exception during authentication check: " + msg)
        false
      case _ => false
    }
  }

  def subscribeEmail(email:String, name: Option[String] = None) = {
    // Instantiate a MailChimpClient
    val mcClient = new MailChimpClient

    // Build the subscribe request
    val listSubscribeCall = new ListSubscribeMethod
    listSubscribeCall.apikey = apiKey
    listSubscribeCall.id = listId
    listSubscribeCall.email_address = email
    listSubscribeCall.update_existing = true

    name.foreach { name =>
      listSubscribeCall.merge_vars = new MergeVars(name)
    }

    // Execute the call against the remote server.
    tryo(mcClient.execute(listSubscribeCall))
  }

  def unsubscribeEmail(email:String) = {
    // Instantiate a MailChimpClient
    val mcClient = new MailChimpClient

    // Build the unsubscribe request
    val listUnsubscribeCall = new ListUnsubscribeMethod
    listUnsubscribeCall.apikey = apiKey
    listUnsubscribeCall.id = listId
    listUnsubscribeCall.email_address = email

    // Execute the call against the remote server
    tryo(mcClient.execute(listUnsubscribeCall))
  }

  def wrapperIdentifier = {
    "MailChimp - " + listId
  }
}
