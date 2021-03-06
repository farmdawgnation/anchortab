package com.anchortab.snippet

import scala.collection.JavaConversions._

import net.liftweb._
  import common._
  import http._
    import provider._
    import js._
      import JsCmds._
    import SHtml._
    import LiftRules._
  import util._
    import Helpers._
  import json._
    import Extraction._
  import mongodb.BsonDSL._

import org.joda.time._

import com.anchortab.model._
import com.anchortab.constantcontact.ConstantContact
import com.anchortab.mailchimp._
import com.anchortab.campaignmonitor._

object OAuth extends Loggable {
  implicit val formats = User.formats

  def dispatch : DispatchPF = {
    case req @ Req("oauth2" :: "constant-contact" :: Nil, _, _) =>
      () => {
        for {
          session <- userSession.is
          code <- req.param("code")
          username <- req.param("username")
          token <- ConstantContact.retrieveAccessTokenForCode(code)
        } yield {
          val serviceCredential = UserServiceCredentials("Constant Contact", username, Map("token" -> token))
          User.update("_id" -> session.userId,
            ("$addToSet" -> (("serviceCredentials" -> decompose(serviceCredential)))) ~
            ("$unset" -> (
              ("firstSteps." + UserFirstStep.Keys.ConnectAnExternalService) -> true
            ))
          )

          Notices.notice("Your Constant Contact account has been successfully connected.")
          RedirectResponse("/manager/services")
        }
      }

    case req @ Req("oauth2" :: "mailchimp" :: Nil, _, _) =>
      () => {
        import com.ecwid.mailchimp._
          import method.helper._

        val mcClient = new MailChimpClient
        val getAccountInfoMethod = new GetAccountDetailsMethod

        // Ain't nobody got time for that
        getAccountInfoMethod.exclude = "modules" :: "orders" :: "rewards-credits" ::
            "rewards-inspections" :: "rewards-referrals" :: "rewards-applied" :: Nil

        for {
          session <- userSession.is
          code <- req.param("code")
          token <- MailchimpOAuth.retrieveAccessTokenAndDcForCode(code)
        } yield {
          getAccountInfoMethod.apikey = token
          val accountInformation = mcClient.execute(getAccountInfoMethod)
          val serviceCredential = UserServiceCredentials("Mailchimp", accountInformation.username, Map("token" -> token))
          User.update("_id" -> session.userId,
            ("$addToSet" -> (("serviceCredentials" -> decompose(serviceCredential)))) ~
            ("$unset" -> (
              ("firstSteps." + UserFirstStep.Keys.ConnectAnExternalService) -> true
            ))
          )

          Notices.notice("Your MailChimp account has been successfully connected.")
          RedirectResponse("/manager/services")
        }
      }

    case req @ Req("oauth2" :: "campaign-monitor" :: Nil, _, _) if req.param("code").isDefined =>
      () => {
        for {
          session <- userSession.is
          code <- req.param("code")
          tokenDetails <- CampaignMonitor.exchangeToken(code)
        } yield {
          val expiresAt = (new DateTime()).plusSeconds(tokenDetails.expires_in)

          val serviceCredential = UserServiceCredentials(
            CampaignMonitor.serviceIdentifier,
            tokenDetails.refresh_token,
            Map(
              "accessToken" -> tokenDetails.access_token,
              "refreshToken" -> tokenDetails.refresh_token,
              "expiresAt" -> decompose(expiresAt).extract[String]
            )
          )

          User.update("_id" -> session.userId,
            ("$addToSet" -> (("serviceCredentials" -> decompose(serviceCredential)))) ~
            ("$unset" -> (
              ("firstSteps." + UserFirstStep.Keys.ConnectAnExternalService) -> true
            ))
          )

          Notices.notice("Your Campaign Monitor account has been successfully connected.")
          RedirectResponse("/manager/services")
        }
      }

    case req @ Req("oauth2" :: serviceIdentifier :: Nil, _, _) if req.param("error").isDefined =>
      () => {
        for {
          error <- req.param("error")
          errorDescription <- req.param("error_description")
        } yield {
          Notices.error("Error Logging In: " + errorDescription)
          RedirectResponse("/manager/services")
        }
      }
  }
}
