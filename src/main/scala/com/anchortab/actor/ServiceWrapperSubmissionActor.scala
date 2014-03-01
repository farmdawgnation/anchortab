package com.anchortab.actor

import scala.collection.JavaConversions._

import net.liftweb._
  import common._
  import actor._
  import json.Extraction._
  import mongodb.BsonDSL._

import com.anchortab.model._

import com.newrelic.api.agent.NewRelic

case class SubscribeEmailToServiceWrapper(
  tab: Tab,
  email: String,
  name: Option[String]
)

object ServiceWrapperSubmissionActor extends LiftActor with Loggable {
  implicit val formats = Tab.formats

  protected def addSubscriberToTab(tab: Tab, email: String, name: Option[String]) = {
    User.update("_id" -> tab.userId,
      "$inc" -> (
        ("quotaCounts." + Plan.Quotas.EmailSubscriptions) -> 1
      )
    )

    Tab.update("_id" -> tab._id, (
      ("$inc" -> ("stats.submissions" -> 1))
    ))
  }

  protected def recordError(tab: Tab, tabError: TabError) = {
    Tab.update("_id" -> tab._id, "$push" -> ("errors" -> decompose(tabError)))
  }

  def messageHandler = {
    case SubscribeEmailToServiceWrapper(tab, email, name) =>
      tab.service match {
        case Some(serviceWrapper) =>
          serviceWrapper.subscribeEmail(email, name) match {
            case Failure(message, _, _) =>
              logger.error("Error submitting email " + email + ":" + message)

              NewRelic.noticeError("External Submission Error", Map(
                "service wrapper" -> serviceWrapper.wrapperIdentifier,
                "email" -> email,
                "error message" -> message
              ))

              recordError(tab, TabError(email, serviceWrapper.wrapperIdentifier, message, name))

            case Empty =>
              logger.error("Got Empty while trying to submit email " + email)

              NewRelic.noticeError("External Submission Empty", Map(
                "service wrapper" -> serviceWrapper.wrapperIdentifier,
                "email" -> email
              ))

              val message = "Something unexpected occured and we didn't get an error."

              recordError(tab, TabError(email, serviceWrapper.wrapperIdentifier, message, name))

            case _ => // Success!
              addSubscriberToTab(tab, email, name)
          }

        case None =>
          addSubscriberToTab(tab, email, name)
      }
  }
}
