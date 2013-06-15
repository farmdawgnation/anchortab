package com.anchortab.snippet

import scala.xml._
import scala.collection.JavaConverters._

import java.text.SimpleDateFormat
import java.util.Date

import net.liftweb._
  import sitemap._
    import Loc._
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
  import mongodb.BsonDSL._

import com.anchortab.model._
import com.anchortab.constantcontact.model.ContactLists._

import com.newrelic.api.agent.NewRelic

import org.bson.types.ObjectId

import org.joda.time._

case class TabEmbedCodeReceived(embedCode: String) extends SimpleAnchorTabEvent("tab-embed-code-received")
case class NewTabCreated(embedCode: String) extends SimpleAnchorTabEvent("new-tab-created")

object Tabs extends Loggable {
  val tabListMenu = Menu.i("Tabs") / "manager" / "tabs"
  val tabNewMenu = Menu.i("New Tab") / "manager" / "tabs" / "new" >>
    TemplateBox(() => Templates("manager" :: "tab" :: "form" :: Nil))
  val tabEditMenu =
    Menu.param[Tab]("Edit Tab", Text("Edit Tab"), Tab.find(_), _._id.toString) /
    "manager" / "tab" / * >>
    TemplateBox(() => Templates("manager" :: "tab" :: "form" :: Nil))
  val tabSubscribersMenu =
    Menu.param[Tab]("Tab Subscribers", Text("Tab Subscribers"), Tab.find(_), _._id.toString) /
    "manager" / "tab" / * / "subscribers" >>
    TemplateBox(() => Templates("manager" :: "tab" :: "subscribers" :: Nil))

  val menus =
    tabListMenu ::
    tabNewMenu ::
    tabEditMenu ::
    tabSubscribersMenu ::
    Nil

  val dateAndTimeFormatter = new SimpleDateFormat("MM/dd/yyyy hh:mm aa")
  val dateFormatter = new SimpleDateFormat("MM/dd/yyyy")

  def snippetHandlers : SnippetPF = {
    case "no-tab-views-help" :: Nil => noTabViewsHelp
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

  def tabName =
    "span *" #> (tabEditMenu.currentValue or tabSubscribersMenu.currentValue).map(_.name)

  def newTabButton = {
    "button [onclick]" #> onEvent(_ => RedirectTo("/manager/tabs/new"))
  }

  def tabList = {
    val tabs = {
      for {
        session <- userSession.is
        userId = session.userId
      } yield {
        Tab.findAll("userId" -> userId)
      }
    } openOr {
      List()
    }

    def getCode(tab:Tab)() =
      TabEmbedCodeReceived(tab.embedCode)

    def subscribers(tabId:ObjectId)() =
      RedirectTo("/manager/tab/" + tabId.toString + "/subscribers")

    def edit(tabId:ObjectId)() =
      RedirectTo("/manager/tab/" + tabId.toString)

    def delete(tabId:ObjectId)() = {
      Tab.delete("_id" -> tabId)

      Notices.notice("Tab deleted.")
      Reload
    }

    ".empty-list" #> (tabs.isEmpty ? PassThru | ClearNodes) andThen
    ".subscriber" #> (tabs.isEmpty ? ClearNodes | PassThru) andThen
    ".subscriber" #> tabs.map { tab =>
      ".subscriber [data-tab-id]" #> tab._id.toString &
      ".tab-name *" #> tab.name &
      ".view-count *" #> tab.stats.views &
      ".subscription-count *" #> tab.stats.submissions &
      ".get-code [onclick]" #> ajaxInvoke(getCode(tab) _) &
      ".subscribers [onclick]" #> ajaxInvoke(subscribers(tab._id) _) &
      ".edit-tab [onclick]" #> ajaxInvoke(edit(tab._id) _) &
      ".delete-tab [onclick]" #> ajaxInvoke(delete(tab._id) _)
    }
  }

  def tabForm = {
    val requestTab = tabEditMenu.currentValue

    var tabName = requestTab.map(_.name) openOr ""
    var appearanceDelay = requestTab.map(_.appearance.delay.toString) openOr ""
    var colorScheme = requestTab.map(_.appearance.colorScheme) openOr TabColorScheme.Red
    var customColorSchemeBase = requestTab.map(_.appearance.colorScheme.baseColor) openOr ""
    var customColorSchemeSecondary = requestTab.map(_.appearance.colorScheme.secondaryColor) openOr ""
    var whitelabel = requestTab.map(_.appearance.whitelabel) openOr false
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
                      whitelabel = whitelabel
                    ),
                    service = serviceWrapper
                  ).save

                  Notices.notice("Tab saved. Go have some juice.")
                  RedirectTo("/manager/tabs")

                case _ =>
                  val tab = Tab(tabName, session.userId,
                      TabAppearance(tryo(appearanceDelay.toInt) openOr 0, customizedColorScheme, customText, whitelabel),
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

    val constantContactLists = {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
        credentials <- user.credentialsFor("Constant Contact")
      } yield {
        implicit val accessToken = credentials.serviceCredentials.get("token") getOrElse ""
        ContactList.findAll match {
          case Full(list) => list
          case Failure(msg, _, _) =>
            NewRelic.noticeError("Constant Contact List List Error", Map(
              "error message" -> msg
            ).asJava)

            logger.error(msg)
            Nil
          case _ => Nil
        }
      }
    } openOr {
      Nil
    }

    val mailChimpAuthorized_? = {
      for {
        session <- userSession.is
        user <- User.find(session.userId)
        credentials <- user.credentialsFor("Mailchimp")
      } yield {
        true
      }
    } openOr {
      false
    }

    val mailchimpLists = {
      import com.ecwid.mailchimp._
        import method.list._

      val lists = {
        for {
          session <- userSession.is
          user <- User.find(session.userId)
          credentials <- user.credentialsFor("Mailchimp")
          token <- credentials.serviceCredentials.get("token")
        } yield {
          val mcClient = new MailChimpClient
          val listsMethod = new ListsMethod
          listsMethod.apikey = token

          mcClient.execute(listsMethod)
        }
      } map(_.data.asScala.toList)

      lists match {
        case Full(mcLists) => mcLists

        case Failure(msg, _, _) =>
          NewRelic.noticeError("MailChimp List List Error", Map(
            "error message" -> msg
          ).asJava)

          Nil

        case _ => Nil
      }
    }

    val campaignMonitorAuthorized_? = {
      import com.anchortab.campaignmonitor._

      for {
        session <- userSession.is
        user <- User.find(session.userId)
        credentials <- user.credentialsFor(CampaignMonitor.serviceIdentifier)
      } yield {
        true
      }
    } openOr {
      false
    }

    val campaignMonitorLists = {
      import com.anchortab.campaignmonitor._

      val lists = {
        for {
          session <- userSession.is
          lists <- CampaignMonitorCredentialsHelper.withAccessCredentials(session.userId) { (accessToken, refreshToken) =>
            CampaignMonitor.getLists(accessToken, refreshToken)
          }
        } yield {
          lists.toList
        }
      }

      lists match {
        case Full(cmLists) => cmLists

        case Failure(msg, _, _) =>
          NewRelic.noticeError("Campaign Monitor List List Error", Map(
            "error message" -> msg
          ).asJava)

          Nil

        case _ => Nil
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

  def subscriberExportLink(ns:NodeSeq) = {
    val transform =
      {
        for {
          tab <- tabSubscribersMenu.currentValue
          tabId = tab._id
        } yield {
          "a [href]" #> ("/manager/tab/" + tabId.toString + "/subscribers/export")
        }
      } openOr {
        "a [href]" #> "#"
      }

    transform(ns)
  }

  def subscriberTable = {
    {
      for {
        requestTab <- tabSubscribersMenu.currentValue
      } yield {
        def deleteSubscriber(email:String)() = {
          Tab.update("_id" -> requestTab._id, "$pull" -> ("subscribers" -> ("email" -> email)))

          Notices.notice("Subscriber deleted.")
          Reload
        }

        val subscriberInformation = requestTab.subscribers.map { subscriber =>
          ".email *" #> subscriber.email &
          ".verified *" #> (subscriber.verified ? "Yes" | "No") &
          ".date *" #> dateAndTimeFormatter.format(subscriber.createdAt.toDate) &
          ".delete-subscriber [onclick]" #> ajaxInvoke(deleteSubscriber(subscriber.email) _)
        }

        if (subscriberInformation.nonEmpty)
          ".subscriber" #> subscriberInformation
        else
          ".subscriber" #> <tr><td colspan="4">No subscribers.</td></tr>
      }
    } openOr {
      throw new ResponseShortcutException(NotFoundResponse("Tab not found."))
    }
  }

  def exportForm = {
    var exportStartDate = dateFormatter.format(new Date())
    var exportEndDate = dateFormatter.format(new Date())

    def submit = {
      Alert("This hasn't been implemented yet.")
    }

    val bind =
      ".export-begin-date" #> text(exportStartDate, exportStartDate = _) &
      ".export-end-date" #> text(exportEndDate, exportEndDate = _) &
      ".export-submit" #> ajaxSubmit("Export", submit _)

    "form" #> { ns:NodeSeq =>
      ajaxForm(bind(ns))
    }
  }
}
