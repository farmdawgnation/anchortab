package com.anchortab.constantcontact.model

import net.liftweb._
  import json._
    import ext._
    import Extraction._
    import JsonDSL._

import org.joda.time._

object Contact {
  val formats = DefaultFormats ++ List(
    ActionBySerializer,
    AddressTypeSerializer,
    ContactListStatusSerializer,
    EmailConfirmStatusSerializer,
    StatusSerializer,
    ContactSerializer
  )

  object ActionBy extends Enumeration {
    val Visitor = Value("ACTION_BY_VISITOR")
    val Owner = Value("ACTION_BY_OWNER")
  }
  object ActionBySerializer extends EnumerationSerializer(ActionBy)

  object AddressType extends Enumeration {
    val Business = Value("business")
    val Personal = Value("personal")
    val Unknown = Value("unknown")
  }
  object AddressTypeSerializer extends EnumerationSerializer(AddressType)

  object ContactListStatus extends Enumeration {
    val Active = Value("ACTIVE")
    val Hidden = Value("HIDDEN")
  }
  object ContactListStatusSerializer extends EnumerationSerializer(ContactListStatus)

  object EmailConfirmStatus extends Enumeration {
    val Confirmed = Value("CONFIRMED")
    val NoConfirmationRequired = Value("NO_CONFIRMATION_REQUIRED")
    val Unconfirmed = Value("UNCONFIRMED")
  }
  object EmailConfirmStatusSerializer extends EnumerationSerializer(EmailConfirmStatus)

  object Status extends Enumeration {
    val Active = Value("ACTIVE")
    val Unconfirmed = Value("UNCONFIRMED")
    val OptOut = Value("OPTOUT")
    val Removed = Value("REMOVED")
    val NonSubscriber = Value("NON_SUBSCRIBER")
    val Visitor = Value("VISITOR")
  }
  object StatusSerializer extends EnumerationSerializer(Status)

  case class Address( address_type:String, line1:String, line2:String, state_code:String,
                      country_code:String, postal_code:String, sub_postal_code:String)

  case class EmailAddress(email_address:String, status:Option[Status.Value] = None,
                          confirm_status:Option[EmailConfirmStatus.Value] = None, opt_in_source:Option[String] = None,
                          opt_in_date:Option[DateTime] = None, opt_out_source:Option[String] = None,
                          opt_out_date:Option[DateTime] = None)

  case class Note(note:String, id:Long, created_on:String)

  case class CustomField(name:String, value:String)

  case class ContactList( id:Long, name:String, contact_count:Int, status:ContactListStatus.Value,
                          opt_in_default:Boolean)

  case class ContactPhones( home_phone:Option[String] = None, work_phone:Option[String] = None,
                            cell_phone:Option[String] = None)
  case class ContactName( first_name:Option[String] = None, middle_name:Option[String] = None,
                          last_name:Option[String] = None)
  case class Contact(email_addresses:List[EmailAddress], action_by:String, id:Long = 0,
                      status:Option[Status.Value] = None, prefix_name:Option[String] = None,
                      name:Option[ContactName] = None, job_title:Option[String] = None,
                      department_name:Option[String] = None, company_name:Option[String] = None,
                      phone:Option[ContactPhones] = None, fax:Option[String] = None,
                      addresses:List[Address] = List(), notes:List[Note] = List(),
                      custom_fields:List[CustomField] = List(), confirmed:Option[Boolean] = None,
                      insert_time:Option[DateTime] = None, last_update_time:Option[DateTime] = None,
                      lists:List[ContactList] = List(), source:Option[String] = None,
                      source_details:Option[String] = None, source_is_url:Option[Boolean] = None,
                      web_url:Option[String] = None)

  object ContactSerializer extends Serializer[Contact] {
    private val Class = classOf[Contact]

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Contact] = {
      case (TypeInfo(Class, _), json) =>
        implicit val formats = DefaultFormats

        val contactName = {
          val extractedName = json.extract[ContactName]
          extractedName match {
            case ContactName(None, None, None) => None
            case _ => Some(extractedName)
          }
        }
        val contactPhone = {
          val extractedPhone = json.extract[ContactPhones]
          extractedPhone match {
            case ContactPhones(None, None, None) => None
            case _ => Some(extractedPhone)
          }
        }
        val contact = json.extract[Contact]

        contact.copy(name = contactName, phone = contactPhone)
    }

    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case x:Contact =>
        // Decompose as a normal case class.
        implicit val formats = DefaultFormats
        var contactJson = decompose(x)

        // Extract names and phone numbers
        val contactNameJson = x.name.map(decompose(_)) getOrElse JObject(Nil)
        val contactPhoneJson = x.phone.map(decompose(_)) getOrElse JObject(Nil)

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
