package com.anchortab.lib

import scala.xml.NodeSeq

import net.liftweb.actor._
import net.liftweb.common._
import com.anchortab.actor.AnchorTabEmailType

trait HandlerChain extends LiftActor with Loggable {
  private val defaultHandler: PartialFunction[Any, Unit] = {
    case somethingUnexpected =>
      logger.warn("Got a message I didn't expect: " + somethingUnexpected)
  }

  private var handlers: List[PartialFunction[Any, Unit]] = Nil

  override lazy val messageHandler: PartialFunction[Any, Unit] =
    handlers.foldRight(defaultHandler)(_ orElse _)

  protected def addHandler(pf: PartialFunction[Any, Unit]) =
    handlers = handlers :+ pf
}

trait EmailHandlerChain extends HandlerChain {
  def sendEmail(subject: String, emails: List[String], nodes: NodeSeq, emailType: AnchorTabEmailType, bccEmails: Boolean = false): Unit
}
