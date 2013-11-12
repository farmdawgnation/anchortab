package com.anchortab.snippet

import scala.xml._

import java.text.SimpleDateFormat
import java.util.Date

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
  import mongodb.BsonDSL._

import com.anchortab.model._

import org.bson.types.ObjectId

import org.joda.time._

case class TabEmbedCodeReceived(embedCode: String) extends SimpleAnchorTabEvent("tab-embed-code-received")
case class NewTabCreated(embedCode: String) extends SimpleAnchorTabEvent("new-tab-created")

object Tabs extends Loggable {
  val tabNewMenu = Menu.i("New Tab") / "manager" / "tabs" / "new" >>
    TemplateBox(() => Templates("manager" :: "tab" :: "form" :: Nil))
  val tabEditMenu =
    Menu.param[Tab]("Edit Tab", Text("Edit Tab"), Tab.find(_), _._id.toString) /
    "manager" / "tab" / * >>
    TemplateBox(() => Templates("manager" :: "tab" :: "form" :: Nil))
  val tabSubscribersMenu =
    Menu.param[Tab]("Tab Subscribers", Text("Tab Subscribers"), Tab.find(_), _._id.toString) /
    "manager" / "tab" / * / "subscribers" >>
    TemplateBox(() => Templates("manager" :: "tab" :: "subscribers" :: Nil))

  val menus =
    tabNewMenu ::
    tabEditMenu ::
    tabSubscribersMenu ::
    Nil

  val dateAndTimeFormatter = new SimpleDateFormat("MM/dd/yyyy hh:mm aa")
  val dateFormatter = new SimpleDateFormat("MM/dd/yyyy")

  def snippetHandlers : SnippetPF = {
    case "no-tab-views-help" :: Nil => noTabViewsHelp
  }

  def noTabViewsHelp = {
    {
      for {
        session <- userSession.is
        numTabs = Tab.count("userId" -> session.userId)
        numEmptyTabs = Tab.count(
          ("userId" -> session.userId) ~
          ("stats.views" -> 0)
        ) if numEmptyTabs == numTabs && numTabs > 0
      } yield {
        PassThru
      }
    } openOr {
      ClearNodes
    }
  }

  def tabName =
    "span *" #> (tabEditMenu.currentValue or tabSubscribersMenu.currentValue).map(_.name)


  def subscriberExportLink(ns:NodeSeq) = {
    val transform =
      {
        for {
          tab <- tabSubscribersMenu.currentValue
          tabId = tab._id
        } yield {
          "a [href]" #> ("/manager/tab/" + tabId.toString + "/subscribers/export")
        }
      } openOr {
        "a [href]" #> "#"
      }

    transform(ns)
  }

  def subscriberTable = {
    {
      for {
        requestTab <- tabSubscribersMenu.currentValue
      } yield {
        def deleteSubscriber(email:String)() = {
          Tab.update("_id" -> requestTab._id, "$pull" -> ("subscribers" -> ("email" -> email)))

          Notices.notice("Subscriber deleted.")
          Reload
        }

        val subscriberInformation = requestTab.subscribers.map { subscriber =>
          ".email *" #> subscriber.email &
          ".verified *" #> (subscriber.verified ? "Yes" | "No") &
          ".date *" #> dateAndTimeFormatter.format(subscriber.createdAt.toDate) &
          ".delete-subscriber [onclick]" #> ajaxInvoke(deleteSubscriber(subscriber.email) _)
        }

        if (subscriberInformation.nonEmpty)
          ".subscriber" #> subscriberInformation
        else
          ".subscriber" #> <tr><td colspan="4">No subscribers.</td></tr>
      }
    } openOr {
      throw new ResponseShortcutException(NotFoundResponse("Tab not found."))
    }
  }

  def exportForm = {
    var exportStartDate = dateFormatter.format(new Date())
    var exportEndDate = dateFormatter.format(new Date())

    def submit = {
      Alert("This hasn't been implemented yet.")
    }

    val bind =
      ".export-begin-date" #> text(exportStartDate, exportStartDate = _) &
      ".export-end-date" #> text(exportEndDate, exportEndDate = _) &
      ".export-submit" #> ajaxSubmit("Export", submit _)

    "form" #> { ns:NodeSeq =>
      ajaxForm(bind(ns))
    }
  }
}
