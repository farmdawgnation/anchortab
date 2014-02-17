package com.anchortab.snippet

import net.liftweb._
  import sitemap._
  import http._
    import js.JsCmds._
    import SHtml._
  import common._
  import util._
    import Helpers._
  import mongodb.BsonDSL._

import com.anchortab.model.{User, Tab, Plan}

import org.bson.types.ObjectId

case class TabEmbedCodeReceived(embedCode: String) extends SimpleAnchorTabEvent("tab-embed-code-received")
case class NewTabCreated(embedCode: String) extends SimpleAnchorTabEvent("new-tab-created")

object TabList {
  val menu = Menu.i("My Tabs") / "manager" / "tabs"
}
class TabList {
  def render = {
    val tabs = {
      for {
        session <- userSession.is
        userId = session.userId
      } yield {
        Tab.findAll("userId" -> userId, "name" -> 1)
      }
    } openOr {
      Nil
    }

    ".empty-list" #> (tabs.isEmpty ? PassThru | ClearNodes) andThen
    ".tab" #> (tabs.isEmpty ? ClearNodes | PassThru) andThen
    ".tab" #> tabs.map { tab =>
      ".tab [data-tab-id]" #> tab._id.toString &
      ".tab-name *" #> tab.name &
      ".subscription-count *" #> tab.stats.submissions &
      ".get-code [onclick]" #> ajaxInvoke(() => {
        TabEmbedCodeReceived(tab.embedCode) 
      }) &
      ".view-errors [onclick]" #> ajaxInvoke(() => {
        RedirectTo(TabErrorsList.menu.toLoc.calcHref(tab))
      }) &
      ".edit-tab [onclick]" #> ajaxInvoke(() => {
        RedirectTo(TabForm.tabEditMenu.toLoc.calcHref(tab))
      }) &
      ".delete-tab [onclick]" #> ajaxInvoke(() => {
        Tab.delete("_id" -> tab._id)

        Notices.notice("Tab deleted.")
        Reload
      }) andThen
      ".view-errors" #> (tab.errors.nonEmpty ? PassThru | ClearNodes)
    }
  }

  def newTabButton = {
    {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
        userTabCount = Tab.count("userId" -> user._id)
      } yield {
        if (user.plan.quotas.get(Plan.Quotas.NumberOfTabs).map(_ > userTabCount) getOrElse true) {
          "button [onclick]" #> onEvent(_ => RedirectTo("/manager/tabs/new"))
        } else {
          "button [class+]" #> "disabled" &
          "button [title]" #> "You've reached the maximum number of tabs allowed for your plan. Please upgrade to add more." &
          "button [rel]" #> "tipsy"
        }
      }
    } openOr {
      ClearNodes
    }
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
}
