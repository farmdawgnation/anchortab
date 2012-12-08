package com.anchortab.snippet

import scala.xml.NodeSeq

import java.text.SimpleDateFormat
import java.util.Date

import net.liftweb._
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

import com.anchortab.model.{Tab, TabAppearance}

import org.bson.types.ObjectId

object requestTabId extends RequestVar[Box[String]](Empty)

object Tabs {
  val dateAndTimeFormatter = new SimpleDateFormat("MM/dd/yyyy hh:mm aa")
  val dateFormatter = new SimpleDateFormat("MM/dd/yyyy")

  def requestTab = requestTabId.is.flatMap(id => Tab.find(new ObjectId(id)))

  def statelessRewrite : RewritePF = {
    case RewriteRequest(ParsePath("manager" :: "tabs" :: "new" :: Nil, _, _, _), _, _) =>
      RewriteResponse("manager" :: "tab" :: "form" :: Nil)

    case RewriteRequest(ParsePath("manager" :: "tab" :: tabId :: "edit" :: Nil, _, _, _), _, _) =>
      requestTabId(Full(tabId))
      RewriteResponse("manager" :: "tab" :: "form" :: Nil)

    case RewriteRequest(ParsePath("manager" :: "tab" :: tabId :: "subscribers" :: Nil, _, _, _), _, _) =>
      requestTabId(Full(tabId))
      RewriteResponse("manager" :: "tab" :: "subscribers" :: Nil)

    case RewriteRequest(ParsePath("manager" :: "tab" :: tabId :: "subscribers" :: "export" :: Nil, _, _, _), _, _) =>
      requestTabId(Full(tabId))
      RewriteResponse("manager" :: "tab" :: "subscribers" :: "export" :: Nil)
  }

  def tabList = {
    val tabs = {
      for {
        session <- userSession.is
        userId = session.userId
      } yield {
        Tab.findAll("userId" -> userId)
      }
    } openOr {
      List()
    }

    def getCode(tab:Tab)() =
      Alert(tab.embedCode)

    def subscribers(tabId:ObjectId)() =
      RedirectTo("/manager/tab/" + tabId.toString + "/subscribers")

    def edit(tabId:ObjectId)() =
      RedirectTo("/manager/tab/" + tabId.toString + "/edit")

    def delete(tabId:ObjectId)() = {
      Tab.delete("_id" -> tabId)

      Alert("Tab deleted.") &
      Reload
    }

    ".empty-list" #> (tabs.isEmpty ? PassThru | ClearNodes) andThen
    ".subscriber" #> (tabs.isEmpty ? ClearNodes | PassThru) andThen
    ".subscriber" #> tabs.map { tab =>
      ".tab-name *" #> tab.name &
      ".view-count *" #> tab.stats.views &
      ".subscription-count *" #> tab.stats.submissions &
      ".get-code [onclick]" #> ajaxInvoke(getCode(tab) _) &
      ".subscribers [onclick]" #> ajaxInvoke(subscribers(tab._id) _) &
      ".edit-tab [onclick]" #> ajaxInvoke(edit(tab._id) _) &
      ".delete-tab [onclick]" #> ajaxInvoke(delete(tab._id) _)
    }
  }

  def tabForm = {
    var tabName = requestTab.map(_.name) openOr ""
    var appearanceDelay = requestTab.map(_.appearance.delay.toString) openOr ""
    var font = requestTab.map(_.appearance.font) openOr ""
    var colorScheme = requestTab.map(_.appearance.colorScheme) openOr ""
    var customText = requestTab.map(_.appearance.customText) openOr ""

    def submit = {
      {
        for {
          session <- userSession.is
        } yield {
          requestTabId.is match {
            case Full(tabId) =>
              Tab.update("_id" -> tabId, "$set" -> (
                ("name" -> tabName) ~
                ("appearance.delay" -> appearanceDelay.toInt) ~
                ("appearance.font" -> font) ~
                ("appearance.colorScheme" -> colorScheme) ~
                ("appearance.customText" -> customText)
              ))

            case _ =>
              val tab = Tab(tabName, session.userId, TabAppearance(appearanceDelay.toInt, font, colorScheme, customText))
              tab.save
          }

          RedirectTo("/manager/tabs")
        }
      } openOr {
        Alert("Something went wrong.")
      }
    }

    val bind =
      "#tab-name" #> text(tabName, tabName = _) &
      "#appearance-delay" #> selectObj[Tab.AppearanceDelayOptions.Value](
        Tab.AppearanceDelayOptions.values.toList.map(v => (v,v.toString)),
        tryo(Tab.AppearanceDelayOptions.withName(appearanceDelay)),
        selected => appearanceDelay = selected.toString
      ) &
      "#font" #> selectObj[Tab.FontOptions.Value](
        Tab.FontOptions.values.toList.map(v => (v,v.toString)),
        tryo(Tab.FontOptions.withName(font)),
        selected => font = selected.toString
      ) &
      "#color-scheme" #> selectObj[Tab.ColorSchemeOptions.Value](
        Tab.ColorSchemeOptions.values.toList.map(v => (v,v.toString)),
        tryo(Tab.ColorSchemeOptions.withName(colorScheme)),
        selected => colorScheme = selected.toString
      ) &
      "#custom-text" #> text(customText, customText = _) &
      ".submit" #> ajaxSubmit("Save Tab", submit _)

    "form" #> { ns:NodeSeq =>
      ajaxForm(bind(ns))
    }
  }

  def subscriberExportLink(ns:NodeSeq) = {
    val transform =
      {
        for {
          tabId <- requestTabId.is
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
        tabId <- requestTabId.is
        requestTab <- Tab.find(tabId)
      } yield {
        def deleteSubscriber(email:String)() = {
          Tab.update("_id" -> tabId, "$pull" -> ("subscribers" -> ("email" -> email)))

          Alert("Subscriber deleted.") &
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
      // TODO
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
