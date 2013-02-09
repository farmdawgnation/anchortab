package com.anchortab.actor

import net.liftweb._
  import common._
  import actor._

import com.anchortab.model.ServiceWrapper

case class SubscribeEmailToServiceWrapper(serviceWrapper: ServiceWrapper, email: String)

object ServiceWrapperSubmissionActor extends LiftActor with Loggable {
  def messageHandler = {
    case SubscribeEmailToServiceWrapper(serviceWrapper, email) =>
      serviceWrapper.subscribeEmail(email) match {
        case Failure(msg, _, _) => logger.error("Error submitting email " + email + ":" + msg)
        case Empty => logger.error("Got Empty while trying to submit email " + email)
        case _ => // Do nothing.
      }
  }
}
