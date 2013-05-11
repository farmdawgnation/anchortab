package com.anchortab.model

import net.liftweb._
  import common._
  import mongodb._
  import util.Helpers._
  import json._

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
  def subscribeEmail(email:String) : Box[Boolean]
  def unsubscribeEmail(email:String) : Box[Boolean]

  def wrapperIdentifier: String
}
object ServiceWrapper {
  val typeHints = ShortTypeHints(classOf[MailChimpServiceWrapper] :: classOf[ConstantContactServiceWrapper] :: Nil)
}

case class CampaignMonitorServiceWrapper(userId: ObjectId, listId: String) extends ServiceWrapper with Loggable {
  import com.anchortab.campaignmonitor._

  protected def updateCredentials(userId: ObjectId, accessToken: String, refreshToken: String, expiresAt: DateTime) = {
    //TODO
  }

  protected def doWithNewAccessCredentials[T](userId: ObjectId, accessToken: String, refreshToken: String, doSomething: (String, String)=>Box[T]): Box[T] = {
    for {
      newToken <- CampaignMonitor.refreshToken(accessToken, refreshToken)
      expiresAt = (new DateTime()).plusSeconds(newToken.expires_in)
      updateCredentialsUnit = updateCredentials(userId, accessToken, refreshToken, expiresAt)
      resultOfSomething <- doSomething(newToken.access_token, newToken.refresh_token)
    } yield {
      resultOfSomething
    }
  }

  protected def withAccessCredentials[T](doSomething: (String, String)=>Box[T]): Box[T] = {
    {
      for {
        user <- (User.find(userId):Box[User])
        credentials <- user.credentialsFor(CampaignMonitor.serviceIdentifier)
        accessToken <- credentials.serviceCredentials.get("accessToken")
        refreshToken <- credentials.serviceCredentials.get("refreshToken")
        expiresAt <- credentials.serviceCredentials.get("expiresAt")
      } yield {
        if (DateTime.parse(expiresAt) isBeforeNow) {
          doWithNewAccessCredentials(userId, accessToken, refreshToken, doSomething)
        } else {
          doSomething(accessToken, refreshToken)
        }
      }
    } match {
      // We unwrap fulls.
      case Full(t) => t
      case fail: Failure => fail
      case _ => Empty
    }
  }

  val credentialsValid_? = true

  def subscribeEmail(email: String) = {
    val result = withAccessCredentials { (accessToken, refreshToken) =>
      CampaignMonitor.addSubscriber(accessToken, refreshToken, listId, email)
    }

    result.map(thing => true)
  }

  def unsubscribeEmail(email: String) = {
    val result = withAccessCredentials { (accessToken, refreshToken) =>
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

  def subscribeEmail(email:String) = {
    val contact = Contact(EmailAddress(email) :: Nil, lists = Some(ContactList(listId.toString) :: Nil))

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

  def wrapperIdentifier = {
    "MailChimp - " + listId
  }
}
