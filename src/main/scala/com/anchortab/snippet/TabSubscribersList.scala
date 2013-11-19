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

import com.anchortab.model.{User, Tab, Plan}

import org.bson.types.ObjectId

object TabSubscribersList {
  val menu =
    Menu.param[Tab]("Tab Subscribers", Text("Tab Subscribers"), Tab.find(_), _._id.toString) /
    "manager" / "tab" / * / "subscribers" >>
    TemplateBox(() => Templates("manager" :: "tab" :: "subscribers" :: Nil))
}
class TabSubscribersList(requestTab: Tab) {
  val dateAndTimeFormatter = new SimpleDateFormat("MM/dd/yyyy hh:mm aa")

  def tabName = "span *" #> requestTab.name

  def render = {
    def deleteSubscriber(email: String)() = {
      Tab.update("_id" -> requestTab._id, "$pull" -> ("subscribers" -> ("email" -> email)))

      Notices.notice("Subscriber deleted.")
      Reload
    }

    val subscriberInformation = requestTab.subscribers.map { subscriber =>
      ".email *" #> subscriber.email &
      ".date *" #> dateAndTimeFormatter.format(subscriber.createdAt.toDate) &
      ".delete-subscriber [onclick]" #> ajaxInvoke(deleteSubscriber(subscriber.email) _)
    }

    ".empty-list" #> (subscriberInformation.nonEmpty ? ClearNodes | PassThru) andThen
    ".subscriber" #> (subscriberInformation.nonEmpty ? PassThru | ClearNodes) andThen
    ".subscriber" #> subscriberInformation
  }
}
