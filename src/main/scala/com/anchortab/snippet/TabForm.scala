package com.anchortab.snippet

import scala.xml.NodeSeq

import net.liftweb._
  import common._
  import http._
    import js._
      import JsCmds._
    import SHtml._
  import util._
    import Helpers._
  import json._
  import mongodb.BsonDSL._

import com.anchortab.model._

class TabForm extends Loggable with MailChimpTabForm with ConstantContactTabForm
              with CampaignMonitorTabForm {
  val requestTab = Tabs.tabEditMenu.currentValue

  var tabName = requestTab.map(_.name) openOr ""
  var appearanceDelay = requestTab.map(_.appearance.delay.toString) openOr ""
  var colorScheme = requestTab.map(_.appearance.colorScheme) openOr TabColorScheme.Red
  var customColorSchemeBase = requestTab.map(_.appearance.colorScheme.baseColor) openOr ""
  var customColorSchemeSecondary = requestTab.map(_.appearance.colorScheme.secondaryColor) openOr ""
  var whitelabel = requestTab.map(_.appearance.whitelabel) openOr false
  var collectName = requestTab.map(_.appearance.collectName) openOr false
  var customText = requestTab.map(_.appearance.customText) openOr ""

  var mailChimpApiKey = ""
  var mailChimpListId: Box[String] = Empty
  var constantContactListId: Box[String] = Empty
  var campaignMonitorListId: Box[String] = Empty

  var leadGenerationTargetEmail: String = ""

  var service : Tab.EmailServices.Value = {
    requestTab.map(_.service).openOr(None) match {
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

      case _ => Tab.EmailServices.None
    }
  }

  val hasWhitelabel_? = {
    {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
        subscription <- user.subscription
        plan <- subscription.plan
          if plan.hasFeature_?(Plan.Features.WhitelabeledTabs)
      } yield {
        true
      }
    } openOr {
      false
    }
  }

  val hasCustomColorSchemes_? = {
    {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
        subscription <- user.subscription
        plan <- subscription.plan
          if plan.hasFeature_?(Plan.Features.CustomColorSchemes)
      } yield {
        true
      }
    } openOr {
      false
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

    none ++ leadGeneration ++ cc ++ mc ++ cm
  }

  def submit = {
    {
      // Build the object that will represent the association between this
      // tab and a remote service.
      val serviceWrapper : Option[ServiceWrapper] = {
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

          case Tab.EmailServices.LeadGeneration =>
            Some(LeadGenerationServiceWrapper(leadGenerationTargetEmail))

          case _ => None
        }
      }

      for {
        session <- userSession.is
      } yield {
        serviceWrapper.map(_.credentialsValid_?) match {
          case Some(false) =>
            GeneralError("Looks like something is wrong with the external service you're connecting to.")

          case _ =>
            val customizedColorScheme = {
              if (colorScheme.name != "Custom")
                colorScheme
              else
                TabColorScheme(customColorSchemeBase, customColorSchemeSecondary, "Custom")
            }

            requestTab match {
              case Full(tab) =>
                tab.copy(
                  name = tabName,
                  appearance = TabAppearance(
                    delay = appearanceDelay.toInt,
                    colorScheme = customizedColorScheme,
                    customText = customText,
                    whitelabel = whitelabel,
                    collectName = collectName
                  ),
                  service = serviceWrapper
                ).save

                Notices.notice("Tab saved. Go have some juice.")
                RedirectTo("/manager/tabs")

              case _ =>
                val tab = Tab(tabName, session.userId,
                    TabAppearance(tryo(appearanceDelay.toInt) openOr 0, customizedColorScheme, customText, whitelabel, collectName),
                    serviceWrapper)
                tab.save

                User.update("_id" -> session.userId, "$unset" -> (
                  ("firstSteps." + UserFirstStep.Keys.CreateATab) -> true)
                )

                Notices.notice("Tab created. Ssssssssmokin'!")
                NewTabCreated(tab.embedCode)
            }
        }
      }
    } openOr {
      GeneralError("Something went wrong. Please contact support by emailing hello@anchortab.com.")
    }
  }

  def render = {
    val bind =
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
        )
      } &
      ".submit" #> ajaxSubmit("Save Tab", submit _)

    "form" #> { ns:NodeSeq =>
      ajaxForm(bind(ns))
    }
  }
}
