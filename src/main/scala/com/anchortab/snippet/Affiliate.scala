package com.anchortab.snippet

import scala.xml._
import scala.collection.immutable.Range._

import net.liftweb._
  import sitemap._
    import Loc._
  import http._
    import js._
      import JsCmds._
    import SHtml._
    import LiftRules._
  import common._
  import util._
    import Helpers._
  import json._
    import JsonDSL._
  import mongodb.BsonDSL._

import com.anchortab.model._

import org.joda.time._

object Affiliate extends Loggable with AffiliateCalculation {
  val menu = Menu.i("Affiliate Info") / "manager" / "affiliate"
  val referralMenu = Menu.param[String]("Referral", Text("Referral"), Full(_), _.toString) /
    "referral" / *

  val menus =
    menu ::
    Nil

  def snippetHandlers: SnippetPF = {
    case "referral-url" :: Nil => referralUrl
    case "referral-performance" :: Nil => referralPerformance
  }

  def referralUrl = {
    val url = {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
        code <- user.affiliateCode
      } yield {
        "http://" + S.hostName + referralMenu.toLoc.calcHref(code)
      }
    }

    ".referral-url *" #> url
  }

  def referralPerformance = {
    val months = (new Inclusive(0, 6, 1)).map { monthsToSubtract =>
      val date = (new DateTime()).minusMonths(monthsToSubtract)

      new YearMonth(date.getYear(), date.getMonthOfYear())
    }

    {
      for {
        session <- userSession.is
      } yield {
        ClearClearable andThen
        ".referral-performance-row" #> months.map { month =>
          ".month *" #> month.toDateTime(new DateMidnight()).toString("MMMM yyyy") &
          ".new *" #> newReferralsForTargetMonth(session.userId, month).total &
          ".total *" #> totalActiveReferralsAsOfTargetMonth(session.userId, month).total
        }
      }
    } openOr {
      ClearNodes
    }
  }
}
