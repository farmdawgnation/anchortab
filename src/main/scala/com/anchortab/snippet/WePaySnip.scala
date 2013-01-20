package com.anchortab.snippet

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
    import JsonDSL._
  import mongodb.BsonDSL._

import me.frmr.wepay._
  import api.Preapproval

import com.anchortab.model._

object WePaySnip extends Loggable {
  implicit val authorizationToken = Authentication.authenticationToken

  def dispatch: DispatchPF = {
    case req @ Req("wepay-redirect" :: Nil, _, _) =>
      () => {
        for {
          preapproval_id_str <- req.param("preapproval_id")
          preapprovalId <- tryo(preapproval_id_str.toLong)
          preapproval <- Preapproval.find(preapprovalId)
          preapprovingUser <- User.find("subscriptions.preapprovalId" -> preapprovalId)
          preapprovedSubscription <- preapprovingUser.subscriptions
            .filter(_.preapprovalId.map(_ == preapprovalId) getOrElse false).headOption
        } yield {
          if (preapproval.state == "approved") {
            preapprovingUser.copy(
              subscriptions = preapprovingUser.subscriptions.map { userSubscription =>
                if (userSubscription._id == preapprovedSubscription._id)
                  userSubscription.copy(status = "active")
                else
                  userSubscription
              }
            ).save

            RedirectResponse("/manager/dashboard")
          } else {
            RedirectResponse("/manager/dashboard")
          }
        }
      }

    case req @ Req("wepay-ipn" :: Nil, _, _) if req.param("preapproval_id").isDefined =>
      () => {
        for {
          preapproval_id_str <- req.param("preapproval_id")
          preapprovalId <- tryo(preapproval_id_str.toLong)
          preapproval <- Preapproval.find(preapprovalId)
          preapprovalState <- preapproval.state
          preapprovingUser <- User.find("subscriptions.preapprovalId" -> preapprovalId)
          preapprovedSubscription <- preapprovingUser.subscriptions
            .filter(_.preapprovalId.map(_ == preapprovalId) getOrElse false).headOption
        } yield {
          def updateSubscriptionState(newStatus: String) = {
            preapprovingUser.copy(
              subscriptions = preapprovingUser.subscriptions.map { userSubscription =>
                if (userSubscription._id == preapprovedSubscription._id)
                  userSubscription.copy(status = newStatus)
                else
                  userSubscription
              }
            ).save
          }

          preapprovalState match {
            case "approved" => updateSubscriptionState("active")
            case "expired" =>
              User.update("_id" -> preapprovingUser._id, "$pull" -> ("subscriptions" ->
                ("preapprovalId" -> preapprovalId)))
            case "stopped" => updateSubscriptionState("stopped")
            case s if s == "cancelled" || s == "revoked" => updateSubscriptionState("cancelled")
          }

          OkResponse()
        }
      }

    case req @ Req("wepay-ipn" :: Nil, _, _) if req.param("checkout_id").isDefined =>
      () => {
        Full(OkResponse())
      }
  }
}
