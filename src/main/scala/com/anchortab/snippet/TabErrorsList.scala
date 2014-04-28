package com.anchortab.snippet

import scala.xml.Text

import java.text.SimpleDateFormat

import net.liftweb._
  import sitemap._
    import Loc._
  import http._
    import js.JsCmds._
    import SHtml._
  import common._
  import util._
    import Helpers._
  import mongodb.BsonDSL._
  import json.Extraction._

import com.anchortab.model.{User, Tab, Plan}

import org.bson.types.ObjectId

object TabErrorsList {
  val menu =
    Menu.param[Tab]("Tab Errors", Text("Tab Errors"), Tab.find(_), _._id.toString) /
    "manager" / "tab" / * / "errors" >>
    TemplateBox(() => Templates("manager" :: "tab" :: "errors" :: Nil)) >>
    Authentication.ifLoggedIn >>
    Authentication.tabIsMine
}
class TabErrorsList(requestTab: Tab) {
  val dateAndTimeFormatter = new SimpleDateFormat("MM/dd/yyyy hh:mm aa")

  def tabName = "span *" #> requestTab.name

  def render = {
    val errorInformation = requestTab.errors.map { error =>
      ".message *" #> error.message &
      ".email *" #> error.email &
      ".time *" #> dateAndTimeFormatter.format(error.createdAt.toDate)
    }

    ".empty-list" #> (errorInformation.nonEmpty ? ClearNodes | PassThru) andThen
    ".error" #> (errorInformation.nonEmpty ? PassThru | ClearNodes) andThen
    ".error" #> errorInformation
  }

  def clearErrors = {
    def doClearErrors(s: String) = {
      implicit val formats = Tab.formats
      Tab.update("_id" -> requestTab._id, "$set" -> ("errors" -> decompose(List())))
      Notices.notice("The errors have been cleared, my leige.")
      RedirectTo(TabList.menu.loc.calcDefaultHref)
    }

    ".clear-errors-button [onclick]" #> onEvent(doClearErrors _)
  }
}
