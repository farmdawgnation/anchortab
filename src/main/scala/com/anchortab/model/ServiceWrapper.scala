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
  def subscribeEmail(email:String) : Box[Boolean]
  def unsubscribeEmail(email:String) : Box[Boolean]
}
object ServiceWrapper {
  val typeHints = ShortTypeHints(List(classOf[MailChimpServiceWrapper]))
}

case class MailChimpServiceWrapper(apiKey:String, listId:String) extends ServiceWrapper {
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
