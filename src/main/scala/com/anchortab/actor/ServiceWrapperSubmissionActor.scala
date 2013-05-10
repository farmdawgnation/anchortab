package com.anchortab.actor

import scala.collection.JavaConversions._

import net.liftweb._
  import common._
  import actor._

import com.anchortab.model.ServiceWrapper

import com.newrelic.api.agent.NewRelic

case class SubscribeEmailToServiceWrapper(serviceWrapper: ServiceWrapper, email: String)

object ServiceWrapperSubmissionActor extends LiftActor with Loggable {
  def messageHandler = {
    case SubscribeEmailToServiceWrapper(serviceWrapper, email) =>
      serviceWrapper.subscribeEmail(email) match {
        case Failure(msg, _, _) =>
          logger.error("Error submitting email " + email + ":" + msg)

          NewRelic.noticeError("External Submission Error", Map(
            "service wrapper" -> serviceWrapper.wrapperIdentifier,
            "email" -> email,
            "error message" -> msg
          ))

        case Empty =>
          logger.error("Got Empty while trying to submit email " + email)

          NewRelic.noticeError("External Submission Empty", Map(
            "service wrapper" -> serviceWrapper.wrapperIdentifier,
            "email" -> email
          ))

        case _ => // Do nothing.
      }
  }
}
