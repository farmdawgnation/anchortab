package com.anchortab.snippet

import scala.xml._

import net.liftweb._
  import common._
  import sitemap._
    import Loc._
  import http._
    import js._
      import JsCmds._
    import SHtml._
  import util._
    import Helpers._
  import json._
  import mongodb.BsonDSL._

import com.anchortab.model._

import org.bson.types.ObjectId

object TabForm {
  val tabNewMenu = Menu.i("New Tab") / "manager" / "tabs" / "new" >>
    TemplateBox(() => Templates("manager" :: "tab" :: "form" :: Nil))
  val tabEditMenu =
    Menu.param[Tab]("Edit Tab", Text("Edit Tab"), Tab.find(_), _._id.toString) /
    "manager" / "tab" / * >>
    TemplateBox(() => Templates("manager" :: "tab" :: "form" :: Nil))
}
class TabForm(requestTab: Tab) extends Loggable
                                          with MailChimpTabForm
                                          with PardotTabForm
                                          with ConstantContactTabForm
                                          with CampaignMonitorTabForm {

  def this() = this(Tab("", ObjectId.get, TabAppearance.defaults))

  var tabName = requestTab.name
  var appearanceDelay = requestTab.appearance.delay.toString
  var colorScheme = requestTab.appearance.colorScheme
  var customColorSchemeBase = requestTab.appearance.colorScheme.baseColor
  var customColorSchemeSecondary = requestTab.appearance.colorScheme.secondaryColor
  var whitelabel = requestTab.appearance.whitelabel
  var collectName = requestTab.appearance.collectName
  var customText = requestTab.appearance.customText

  var mailChimpApiKey = ""
  var mailChimpListId: Box[String] = Empty
  var constantContactListId: Box[String] = Empty
  var campaignMonitorListId: Box[String] = Empty

  var pardotTargetUri: String = ""
  var pardotEmailField: String = ""
  var pardotNameField: String = ""

  var leadGenerationTargetEmail: String = ""

  var service : Tab.EmailServices.Value = {
    requestTab.service match {
      case Some(mcsw:MailChimpServiceWrapper) =>
        mailChimpListId = Full(mcsw.listId)

        Tab.EmailServices.MailChimp

      case Some(ccsw:ConstantContactServiceWrapper) =>
        constantContactListId = Full(ccsw.listId.toString)

        Tab.EmailServices.ConstantContact

      case Some(cmsw: CampaignMonitorServiceWrapper) =>
        campaignMonitorListId = Full(cmsw.listId)

        Tab.EmailServices.CampaignMonitor

      case Some(lgsw: LeadGenerationServiceWrapper) =>
        leadGenerationTargetEmail = lgsw.targetEmail

        Tab.EmailServices.LeadGeneration

      case Some(pdsw: PardotServiceWrapper) =>
        pardotTargetUri = pdsw.targetUri
        pardotEmailField = pdsw.emailFieldName
        pardotNameField = pdsw.firstNameFieldName

        Tab.EmailServices.Pardot

      case _ => Tab.EmailServices.None
    }
  }

  val (hasWhitelabel_?, hasCustomColorSchemes_?) = {
    {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
        subscription <- user.subscription
        plan <- subscription.plan
      } yield {
        (
          plan.hasFeature_?(Plan.Features.WhitelabeledTabs),
          plan.hasFeature_?(Plan.Features.CustomColorSchemes)
        )
      }
    } openOr {
      (false, false)
    }
  }

  val colorSchemeList =
    if (hasCustomColorSchemes_?)
      TabColorScheme.advanced
    else
      TabColorScheme.basic

  val validEmailServices = {
    val none = List(Tab.EmailServices.None)
    val leadGeneration = List(Tab.EmailServices.LeadGeneration)
    val cc = (constantContactLists.nonEmpty ? List(Tab.EmailServices.ConstantContact) | List())
    val mc = (mailChimpAuthorized_? ? List(Tab.EmailServices.MailChimp) | List())
    val cm = (campaignMonitorAuthorized_? ? List(Tab.EmailServices.CampaignMonitor) | List())
    val pd = (pardotAuthorized_? ? List(Tab.EmailServices.Pardot) | List())

    none ++ leadGeneration ++ cc ++ mc ++ cm ++ pd
  }

  def serviceWrapper : Option[ServiceWrapper] = {
    service match {
      case Tab.EmailServices.MailChimp =>
        for {
          session <- userSession.is
          user <- User.find(session.userId)
          credentials <- user.credentialsFor("Mailchimp")
          token <- credentials.serviceCredentials.get("token")
          mailChimpListId <- mailChimpListId
        } yield {
          MailChimpServiceWrapper(token, mailChimpListId)
        }

      case Tab.EmailServices.ConstantContact =>
        for {
          session <- userSession.is
          user <- User.find(session.userId)
          credentials <- user.credentialsFor("Constant Contact")
          token <- credentials.serviceCredentials.get("token")
          constantContactListId <- constantContactListId
        } yield {
          ConstantContactServiceWrapper(credentials.userIdentifier, token, constantContactListId.toLong)
        }

      case Tab.EmailServices.CampaignMonitor =>
        for {
          session <- userSession.is
          listId <- campaignMonitorListId
        } yield {
          CampaignMonitorServiceWrapper(session.userId, listId)
        }

      case Tab.EmailServices.Pardot =>
        for {
          session <- userSession.is
        } yield {
          PardotServiceWrapper(session.userId, pardotTargetUri, pardotEmailField, pardotNameField)
        }

      case Tab.EmailServices.LeadGeneration =>
        Some(LeadGenerationServiceWrapper(leadGenerationTargetEmail))

      case _ => None
    }
  }

  def submit = {
    {
      for {
        session <- userSession.is
      } yield {
        val customizedColorScheme = {
          if (colorScheme.name != "Custom")
            colorScheme
          else
            TabColorScheme.Custom.copy(
              baseColor = customColorSchemeBase,
              secondaryColor = customColorSchemeSecondary
            )
        }

        requestTab.copy(
          name = tabName,
          userId = session.userId,
          appearance = TabAppearance(
            delay = appearanceDelay.toInt,
            colorScheme = customizedColorScheme,
            customText = customText,
            whitelabel = whitelabel,
            collectName = collectName
          ),
          service = serviceWrapper
        ).save

        if(requestTab.userId != session.userId) {
          User.update("_id" -> session.userId, "$unset" -> (
            ("firstSteps." + UserFirstStep.Keys.CreateATab) -> true)
          )

          Notices.notice("Tab created. Ssssssssmokin'!")
          NewTabCreated(requestTab.embedCode)
        } else {
          Notices.notice("Tab saved. Go have some juice.")
          RedirectTo(TabList.menu.loc.calcDefaultHref)
        }
      }
    } openOr {
      GeneralError("Something went wrong. Please contact support by emailing hello@anchortab.com.")
    }
  }

  def render = {
    SHtml.makeFormsAjax andThen
    "#tab-name" #> text(tabName, tabName = _) &
    "#appearance-delay" #> selectObj[Tab.AppearanceDelayOptions.Value](
      Tab.AppearanceDelayOptions.values.toList.map(v => (v,v.toString)),
      tryo(Tab.AppearanceDelayOptions.withName(appearanceDelay)),
      selected => appearanceDelay = selected.toString
    ) &
    "#color-scheme" #> select(
      colorSchemeList.map(v => (v.toString,v.toString)),
      Full(colorScheme.toString),
      (selectedSchemeName: String) => {
        selectedSchemeName match {
          case TabColorScheme.Custom.toString if hasCustomColorSchemes_? =>
            colorScheme = TabColorScheme.Custom
          case TabColorScheme.Red.toString =>
            colorScheme = TabColorScheme.Red
          case TabColorScheme.Green.toString =>
            colorScheme = TabColorScheme.Green
          case TabColorScheme.Blue.toString =>
            colorScheme = TabColorScheme.Blue
          case _ =>
            colorScheme = TabColorScheme.Gray
        }
      }
    ) &
    ".custom-color-group" #> (hasCustomColorSchemes_? ? PassThru | ClearNodes) andThen
    "#custom-color-scheme-base" #> text(customColorSchemeBase, customColorSchemeBase = _, ("type" -> "color")) &
    "#custom-color-scheme-secondary" #> text(customColorSchemeSecondary, customColorSchemeSecondary = _, ("type" -> "color")) &
    ".whitelabel-group" #> (hasWhitelabel_? ? PassThru | ClearNodes) andThen
    "#whitelabel" #> checkbox(whitelabel, whitelabel = _) &
    "#collect-name" #> checkbox(collectName, collectName = _) &
    "#custom-text" #> text(customText, customText = _) &
    "#email-marketing-service-selection" #> idMemoize { renderer =>
      "#service" #> ajaxSelectObj[Tab.EmailServices.Value](
        validEmailServices.map(v => (v,v.toString)),
        Full(service),
        { selected: Tab.EmailServices.Value =>
          if (selected != service) {
            service = selected
            renderer.setHtml
          } else {
            Noop
          }
        }
      ) &
      ".only-if-lead-generation" #> {
        if (service == Tab.EmailServices.LeadGeneration)
          PassThru
        else
          ClearNodes
      } andThen
      "#lead-generation-target-email" #> text(leadGenerationTargetEmail, leadGenerationTargetEmail = _) &
      ".only-if-mailchimp-authorized" #> {
        if (service == Tab.EmailServices.MailChimp && mailChimpAuthorized_?)
          PassThru
        else
          ClearNodes
      } andThen
      "#mailchimp-listid" #> select(
        mailchimpLists.map(l => (l.id, l.name)),
        mailChimpListId,
        id => mailChimpListId = Full(id)
      ) &
      ".only-if-constantcontact-authorized" #> {
        if (service == Tab.EmailServices.ConstantContact && constantContactLists.nonEmpty)
          PassThru
        else
          ClearNodes
      } andThen
      "#constantcontact-listid" #> select(
        constantContactLists.map(l => (l.id.toString, l.name.getOrElse(""))),
        constantContactListId,
        id => constantContactListId = Full(id)
      ) &
      ".only-if-campaignmonitor-authorized" #> {
        if(service == Tab.EmailServices.CampaignMonitor && campaignMonitorAuthorized_?)
          PassThru
        else
          ClearNodes
      } andThen
      "#campaignmonitor-listid" #> select(
        campaignMonitorLists.map(l => (l.id.toString, l.name)),
        campaignMonitorListId,
        id => campaignMonitorListId = Full(id)
      ) andThen
      ".only-if-pardot-authorized" #> {
        if (service == Tab.EmailServices.Pardot && pardotAuthorized_?) {
          PassThru
        } else {
          ClearNodes
        }
      } andThen
      "#pardot-target-uri" #> text(pardotTargetUri, pardotTargetUri = _) &
      "#pardot-email-field-name" #> text(pardotEmailField, pardotEmailField = _) &
      "#pardot-first-name-field-name" #> text(pardotNameField, pardotNameField = _)
    } &
    ".submit" #> ajaxSubmit("Save Tab", submit _)
  }
}
