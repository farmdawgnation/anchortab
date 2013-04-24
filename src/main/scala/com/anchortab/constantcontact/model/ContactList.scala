package com.anchortab.constantcontact.model

import net.liftweb._
  import common._
  import json._
    import ext._
    import Extraction._
    import JsonDSL._
  import util._
    import Helpers._

import org.joda.time._

import com.anchortab.constantcontact.ConstantContact

object ContactLists {
  implicit val formats = DefaultFormats

  object ContactListStatus {
    val Active = "ACTIVE"
    val Hidden = "HIDDEN"
  }

  case class ContactList( id:String, name:String, contact_count:Int, status:String) {
    def delete(implicit accessToken:String) = {
      ContactList.delete(id)
    }

    def save(implicit accessToken:String) = {
      ContactList.save(decompose(this), id)
    }
  }
  object ContactList {
    def find(id:String)(implicit accessToken:String) = {
      ConstantContact.get("lists/" + id).flatMap { json =>
        tryo(json.extract[ContactList])
      }
    }

    def findAll(implicit accessToken:String) = {
      ConstantContact.get("lists").flatMap { json =>
        tryo(json.extract[List[ContactList]])
      }
    }

    def delete(id:String)(implicit accessToken:String) = {
      ConstantContact.delete("lists/" + id).flatMap { json=>
        tryo(json.extract[Boolean])
      }
    }

    def save(contactListJson:JValue, id:String)(implicit accessToken:String) = {
      Failure("Not implemented.")
    }
  }
}
