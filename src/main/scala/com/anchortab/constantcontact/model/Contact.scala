package com.anchortab.constantcontact.model

import net.liftweb._
  import json._
    import ext._
    import Extraction._
    import JsonDSL._

import org.joda.time._

object Contact {
  val formats = List(
    ActionBySerializer,
    AddressTypeSerializer,
    ContactListStatusSerializer,
    EmailConfirmStatusSerializer,
    StatusSerializer,
    ContactSerializer
  )

  val nonContactFormats = List(
    ActionBySerializer,
    AddressTypeSerializer,
    ContactListStatusSerializer,
    EmailConfirmStatusSerializer,
    StatusSerializer
  )

  object ActionBy extends Enumeration {
    val Visitor = Value("ACTION_BY_VISITOR")
    val Owner = Value("ACTION_BY_OWNER")
  }
  object ActionBySerializer extends Serializer[ActionBy.Value] {
    private val Class = classOf[ActionBy.Value]

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), ActionBy.Value] = {
      case (TypeInfo(Class, _), json) =>
        val stringValue = json.extract[String]
        ActionBy.withName(stringValue)
    }

    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case x:ActionBy.Value => x.toString
    }
  }

  object AddressType extends Enumeration {
    val Business = Value("business")
    val Personal = Value("personal")
    val Unknown = Value("unknown")
  }
  object AddressTypeSerializer extends Serializer[AddressType.Value] {
    private val Class = classOf[AddressType.Value]

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), AddressType.Value] = {
      case (TypeInfo(Class, _), json) =>
        val stringValue = json.extract[String]
        AddressType.withName(stringValue)
    }

    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case x:AddressType.Value => x.toString
    }
  }

  object ContactListStatus extends Enumeration {
    val Active = Value("ACTIVE")
    val Hidden = Value("HIDDEN")
  }
  object ContactListStatusSerializer extends Serializer[ContactListStatus.Value] {
    private val Class = classOf[ContactListStatus.Value]

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), ContactListStatus.Value] = {
      case (TypeInfo(Class, _), json) =>
        val stringValue = json.extract[String]
        ContactListStatus.withName(stringValue)
    }

    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case x:ContactListStatus.Value => x.toString
    }
  }

  object EmailConfirmStatus extends Enumeration {
    val Confirmed = Value("CONFIRMED")
    val NoConfirmationRequired = Value("NO_CONFIRMATION_REQUIRED")
    val Unconfirmed = Value("UNCONFIRMED")
  }
  object EmailConfirmStatusSerializer extends Serializer[EmailConfirmStatus.Value] {
    private val Class = classOf[EmailConfirmStatus.Value]

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), EmailConfirmStatus.Value] = {
      case (TypeInfo(Class, _), json) =>
        val stringValue = json.extract[String]
        EmailConfirmStatus.withName(stringValue)
    }

    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case x:EmailConfirmStatus.Value => x.toString
    }
  }

  object Status extends Enumeration {
    val Active = Value("ACTIVE")
    val Unconfirmed = Value("UNCONFIRMED")
    val OptOut = Value("OPTOUT")
    val Removed = Value("REMOVED")
    val NonSubscriber = Value("NON_SUBSCRIBER")
    val Visitor = Value("VISITOR")
  }
  object StatusSerializer extends Serializer[Status.Value] {
    private val Class = classOf[Status.Value]

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Status.Value] = {
      case (TypeInfo(Class, _), json) =>
        val stringValue = json.extract[String]
        Status.withName(stringValue)
    }

    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case x:Status.Value => x.toString
    }
  }

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
  case class Contact(email_addresses:List[EmailAddress], action_by:ActionBy.Value, id:Long = 0,
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
        implicit val formats = DefaultFormats ++ nonContactFormats

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
        implicit val formats = DefaultFormats ++ nonContactFormats
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
