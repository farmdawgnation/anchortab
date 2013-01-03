package com.anchortab.constantcontact.model

import net.liftweb._
  import json._
    import ext._
    import Extraction._
    import JsonDSL._
  import util._
    import Helpers._

import org.joda.time._

import com.anchortab.constantcontact.ConstantContact

object Contacts {
  implicit val formats = (DefaultFormats + ContactSerializer) ++ JodaTimeSerializers.all

  val nonContactFormats = DefaultFormats ++ JodaTimeSerializers.all

  object ActionBy {
    val Visitor = "ACTION_BY_VISITOR"
    val Owner = "ACTION_BY_OWNER"
  }

  object AddressType {
    val Business = "BUSINESS"
    val Personal = "PERSONAL"
    val Unknown = "UNKNOWN"
  }

  object ContactListStatus {
    val Active = "ACTIVE"
    val Hidden = "HIDDEN"
  }

  object EmailConfirmStatus {
    val Confirmed = "CONFIRMED"
    val NoConfirmationRequired = "NO_CONFIRMATION_REQUIRED"
    val Unconfirmed = "UNCONFIRMED"
  }

  object Status {
    val Active = "ACTIVE"
    val Unconfirmed = "UNCONFIRMED"
    val OptOut = "OPTOUT"
    val Removed = "REMOVED"
    val NonSubscriber = "NON_SUBSCRIBER"
    val Visitor = "VISITOR"
  }

  case class Address( address_type:String, line1:String, line2:String, line3:String, state_code:String,
                      country_code:String, postal_code:String, sub_postal_code:String)

  case class EmailAddress(email_address:String, status:Option[String] = None,
                          confirm_status:Option[String] = None, opt_in_source:Option[String] = None,
                          opt_in_date:Option[DateTime] = None, opt_out_source:Option[String] = None,
                          opt_out_date:Option[DateTime] = None)

  case class Note(note:String, id:Long, created_on:String)

  case class CustomField(name:String, value:String)

  case class ContactList( id:Long, name:String, contact_count:Int, status:String,
                          opt_in_default:Boolean)

  case class ContactPhones( home_phone:Option[String] = None, work_phone:Option[String] = None,
                            cell_phone:Option[String] = None)
  case class ContactName( first_name:Option[String] = None, middle_name:Option[String] = None,
                          last_name:Option[String] = None)

  object Contact {
    def find(id:Long)(implicit accessToken:String) = {
      ConstantContact.get("contacts/" + id).flatMap { json =>
        tryo(json.extract[Contact])
      }
    }

    def find(email:String)(implicit accessToken:String) = {
      ConstantContact.get("contacts", Map("email" -> email)).flatMap { json =>
        tryo(json.extract[Contact])
      }
    }

    def delete(id:Long)(implicit accessToken:String) = {
      ConstantContact.delete("contacts/" + id).flatMap { json =>
        tryo(json.extract[Boolean])
      }
    }

    def save(contactJson:JValue, id:Option[Long] = None)(implicit accessToken:String) = {
      id match {
        case None =>
          ConstantContact.post("contacts", contactJson).flatMap{ json =>
            tryo(json.extract[Contact])
          }

        case Some(id) =>
          ConstantContact.put("contacts/" + id, contactJson).flatMap { json =>
            tryo(json.extract[Contact])
          }
      }
    }

    def addToLists(listIds:List[Long], id:Long = 0)(implicit accessToken:String) = {
      val listIdsJson = listIds.map { listId =>
        ("id" -> listId)
      }

      ConstantContact.post("contacts/" + id + "/lists", listIdsJson)
    }
  }
  case class Contact(email_addresses:List[EmailAddress], action_by:String, id:Option[Long] = None,
                      status:Option[String] = None, prefix_name:Option[String] = None,
                      name:Option[ContactName] = None, job_title:Option[String] = None,
                      department_name:Option[String] = None, company_name:Option[String] = None,
                      phone:Option[ContactPhones] = None, fax:Option[String] = None,
                      addresses:Option[List[Address]] = None, notes:Option[List[Note]] = None,
                      custom_fields:Option[List[CustomField]] = None, confirmed:Option[Boolean] = None,
                      insert_time:Option[DateTime] = None, last_update_time:Option[DateTime] = None,
                      lists:Option[List[ContactList]] = None, source:Option[String] = None,
                      source_details:Option[String] = None, source_is_url:Option[Boolean] = None,
                      web_url:Option[String] = None) {
    def delete(implicit accessToken:String) = {
      id.flatMap(Contact.delete(_))
    }

    def save(implicit accessToken:String) = {
      Contact.save(decompose(this), id)
    }

    def addToLists(listIds:List[Long])(implicit accessToken:String) = {
      id.flatMap(Contact.addToLists(listIds, _))
    }

  }

  object ContactSerializer extends Serializer[Contact] {
    private val Class = classOf[Contact]

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Contact] = {
      case (TypeInfo(Class, _), json) =>
        implicit val formats = nonContactFormats

        val contactName = {
          val extractedName = json.extract[ContactName](formats, manifest[ContactName])
          extractedName match {
            case ContactName(None, None, None) => None
            case _ => Some(extractedName)
          }
        }
        val contactPhone = {
          val extractedPhone = json.extract[ContactPhones](formats, manifest[ContactPhones])
          extractedPhone match {
            case ContactPhones(None, None, None) => None
            case _ => Some(extractedPhone)
          }
        }
        val contact = json.extract[Contact](formats, manifest[Contact])

        contact.copy(name = contactName, phone = contactPhone)
    }

    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case x:Contact =>
        // Decompose as a normal case class.
        implicit val formats = nonContactFormats
        var contactJson = decompose(x)(formats)

        // Extract names and phone numbers
        val contactNameJson = x.name.map(decompose(_)(formats)) getOrElse JObject(Nil)
        val contactPhoneJson = x.phone.map(decompose(_)(formats)) getOrElse JObject(Nil)

        val filteredJvalue = contactJson.remove { contactfield =>
          contactfield match {
            case JField("name", _) => true
            case JField("phone", _) => true
            case _ => false
          }
        }

        (filteredJvalue, contactNameJson, contactPhoneJson) match {
          case (filteredObject:JObject, contactNameJson:JObject, contactPhoneJson:JObject) =>
            filteredObject ~
            contactNameJson ~
            contactPhoneJson

          case _ => JObject(Nil)
        }
    }
  }
}
