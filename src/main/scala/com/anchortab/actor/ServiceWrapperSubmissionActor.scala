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
  protected def addSubscriberToTab(tab: Tab, email: String, name: Option[String]) = {
    implicit val formats = Tab.formats

    val subscriberInformation = TabSubscriber(email, name)

    User.update("_id" -> tab.userId,
      "$inc" -> (
        ("quotaCounts." + Plan.Quotas.EmailSubscriptions) -> 1
      )
    )

    Tab.update("_id" -> tab._id, (
      ("$inc" -> ("stats.submissions" -> 1)) ~
      ("$addToSet" -> ("subscribers" -> decompose(subscriberInformation)))
    ))
  }

  def messageHandler = {
    case SubscribeEmailToServiceWrapper(tab, email, name) =>
      tab.service match {
        case Some(serviceWrapper) =>
          serviceWrapper.subscribeEmail(email, name) match {
            case Failure(msg, _, _) =>
              logger.error("Error submitting email " + email + ":" + msg)

              NewRelic.noticeError("External Submission Error", Map(
                "service wrapper" -> serviceWrapper.wrapperIdentifier,
                "email" -> email,
                "error message" -> msg
              ))

              EventActor ! TrackEvent(Event(
                eventType = Event.Types.TabSubmitError,
                userId = Some(tab.userId),
                tabId = Some(tab._id),
                email = Some(email),
                service = Some(serviceWrapper.wrapperIdentifier),
                message = Some(msg)
              ))

            case Empty =>
              logger.error("Got Empty while trying to submit email " + email)

              NewRelic.noticeError("External Submission Empty", Map(
                "service wrapper" -> serviceWrapper.wrapperIdentifier,
                "email" -> email
              ))

              EventActor ! TrackEvent(Event(
                eventType = Event.Types.TabSubmitError,
                userId = Some(tab.userId),
                tabId = Some(tab._id),
                email = Some(email),
                service = Some(serviceWrapper.wrapperIdentifier)
              ))

            case _ => // Success!
              addSubscriberToTab(tab, email, name)
          }

        case None =>
          addSubscriberToTab(tab, email, name)
      }
  }
}
