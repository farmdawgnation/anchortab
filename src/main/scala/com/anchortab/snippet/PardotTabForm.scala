package com.anchortab.snippet

import scala.collection.JavaConverters._

import net.liftweb._
  import common._

import com.anchortab.model._

import com.ecwid.mailchimp._
  import method.list._

import com.newrelic.api.agent.NewRelic

trait PardotTabForm {
  val pardotAuthorized_? = {
    for {
      session <- userSession.is
      user <- User.find(session.userId)
      credentials <- user.credentialsFor("Pardot") if user.plan.hasFeature_?(Plan.Features.PardotIntegration)
    } yield {
      true
    }
  } openOr {
    false
  }
}
