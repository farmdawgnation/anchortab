package com.anchortab.model

import net.liftweb._
  import common._
  import mongodb._
  import util.Helpers._
  import json._

import org.joda.time._

import com.ecwid.mailchimp._
  import method.list._

/**
 * The ServiceWrapper trait is the common interface for all wrappers around external email
 * subscription services. The only two functions these wrappers are required to expose are
 * a subscribe and an unsubscribe. The mechanics and individual settings of how they handle
 * those operations are irrelevent to the parent interface.
**/
sealed trait ServiceWrapper {
  def credentialsValid_? : Boolean
  def subscribeEmail(email:String) : Box[Boolean]
  def unsubscribeEmail(email:String) : Box[Boolean]
}
object ServiceWrapper {
  val typeHints = ShortTypeHints(classOf[MailChimpServiceWrapper] :: classOf[ConstantContactServiceWrapper] :: Nil)
}

case class ConstantContactServiceWrapper(username:String, implicit val accessToken:String, listId:Int) extends ServiceWrapper with Loggable {
  import com.anchortab.constantcontact.model.Contacts._

  // This is an OAuth-based API wrapper, making the checking of valid credentials unneeded for the moment
  val credentialsValid_? = true

  def subscribeEmail(email:String) = {
    val contact = Contact(EmailAddress(email) :: Nil, ActionBy.Visitor)

    for {
      contact <- contact.save
      listResult <- contact.addToLists(listId.toLong :: Nil)
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

  def subscribeEmail(email:String) = {
    // Instantiate a MailChimpClient
    val mcClient = new MailChimpClient

    // Build the subscribe request
    val listSubscribeCall = new ListSubscribeMethod
    listSubscribeCall.apikey = apiKey
    listSubscribeCall.id = listId
    listSubscribeCall.email_address = email
    listSubscribeCall.update_existing = true

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
}
