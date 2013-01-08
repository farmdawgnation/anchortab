package com.anchortab.snippet

import scala.xml.NodeSeq

import java.text.SimpleDateFormat

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
    import Extraction._
  import mongodb.BsonDSL._

import com.anchortab.model.{User, UserProfile}

import org.bson.types.ObjectId

object Admin {
  val shortDateFormatter = new SimpleDateFormat("MM/dd/yyyy")
  val longDateFormatter = new SimpleDateFormat("MMMM dd, yyyy")

  def snippetHandlers : SnippetPF = {
    case "admin-user-list" :: Nil => adminUserList _
  }

  def adminUserList(xhtml:NodeSeq) = {
    def editUser(userId:ObjectId)(s:String) = {
      RedirectTo("/admin/user/" + userId.toString)
    }

    val userListTransform =
      {
        for {
          session <- userSession.is
          currentUser <- User.find(session.userId) if currentUser.admin_?
          users = User.findAll
        } yield {
          ".user-row" #> users.map { user =>
            ".email *" #> user.email &
            ".name *" #> user.name &
            ".edit-user [onclick]" #> onEvent(editUser(user._id) _)
          }
        }
      }

    userListTransform.map(_.apply(xhtml)) openOr NodeSeq.Empty
  }
}
