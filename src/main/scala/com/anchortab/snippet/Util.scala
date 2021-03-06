package com.anchortab.snippet

import scala.xml._

import net.liftweb._
  import common._
  import http._
    import SHtml._
    import LiftRules._
    import rest._
    import js._
      import JsCmds._
      import JE._
      import JsExp._
  import util._
    import Helpers._
  import json._
    import JsonDSL._
    import Extraction._
  import mongodb._
    import BsonDSL._

import com.newrelic.api.agent._

import com.anchortab.model.User

class SimpleAnchorTabEvent(eventName:String) extends JsCmd {
  import Serialization._

  implicit def typeHints = Serialization.formats(NoTypeHints)

  def toJsCmd = {
    Call("anchortabSite.event", eventName, decompose(this)).cmd.toJsCmd
  }
}
class AnchorTabEvent(eventName:String, parameters:JObject) extends JsCmd {
  def toJsCmd = {
    Call("anchortabSite.event", eventName, parameters).cmd.toJsCmd
  }
}

class CallableFunction(name:String, callback:(String)=>JsCmd, args:List[String] = List()) extends JsCmd {
  override val toJsCmd =
    Function(
      name, args,
      ajaxCall(JsRaw("Array.prototype.slice.call(arguments).join('|')"), callback)._2
    ).toJsCmd
}

class AnonCallableFunction(callback:(String)=>JsCmd, args:List[String] = List()) extends JsCmd {
  override val toJsCmd =
    AnonFunc(
      args.mkString(","),
      ajaxCall(JsRaw("Array.prototype.slice.call(arguments).join('|')"), callback)._2
    ).toJsCmd
}

object currentPageTitle extends RequestVar[Box[String]](Empty)

object Util extends Loggable {
  def snippetHandlers : SnippetPF = {
    case "ie-conditional" :: Nil => ieConditional _
    case "jquery" :: Nil => jquery _
    case "set-page-title" :: Nil => setPageTitle _
    case "page-title" :: Nil => pageTitle _
    case "highlight-active-navigation" :: Nil => highlightActiveNavigation _
    case "user-gravatar" :: Nil => userGravatar _
    case "user-name" :: Nil => userName _
    case "google-analytics" :: Nil => googleAnalytics _
    case "first-steps" :: Nil => firstSteps
    case "newrelic-browser-timing-header" :: Nil => newrelicBrowserTimingHeader
    case "newrelic-browser-timing-footer" :: Nil => newrelicBrowserTimingFooter
    case "mixpanel" :: Nil => mixpanel _
  }

  def mixpanel(xhtml: NodeSeq) = {
    val mixpanelKey = Props.get("mixpanel.key") openOr "ca4b2921b5c4b15cf8a8b8cfa661a3aa"

    Unparsed("""
      <!-- start Mixpanel --><script type="text/javascript">(function(e,b){if(!b.__SV){var a,f,i,g;window.mixpanel=b;a=e.createElement("script");a.type="text/javascript";a.async=!0;a.src=("https:"===e.location.protocol?"https:":"http:")+'//cdn.mxpnl.com/libs/mixpanel-2.2.min.js';f=e.getElementsByTagName("script")[0];f.parentNode.insertBefore(a,f);b._i=[];b.init=function(a,e,d){function f(b,h){var a=h.split(".");2==a.length&&(b=b[a[0]],h=a[1]);b[h]=function(){b.push([h].concat(Array.prototype.slice.call(arguments,0)))}}var c=b;"undefined"!==
      typeof d?c=b[d]=[]:d="mixpanel";c.people=c.people||[];c.toString=function(b){var a="mixpanel";"mixpanel"!==d&&(a+="."+d);b||(a+=" (stub)");return a};c.people.toString=function(){return c.toString(1)+".people (stub)"};i="disable track track_pageview track_links track_forms register register_once alias unregister identify name_tag set_config people.set people.set_once people.increment people.append people.track_charge people.clear_charges people.delete_user".split(" ");for(g=0;g<i.length;g++)f(c,i[g]);
      b._i.push([a,e,d])};b.__SV=1.2}})(document,window.mixpanel||[]);
      mixpanel.init("""" + mixpanelKey + """");</script><!-- end Mixpanel -->
    """)
  }

  def newrelicBrowserTimingFooter = {
    if (Props.productionMode) {
      "script" #> Unparsed(NewRelic.getBrowserTimingFooter())
    } else {
      ClearNodes
    }
  }

  def newrelicBrowserTimingHeader = {
    if (Props.productionMode) {
      "script" #> Unparsed(NewRelic.getBrowserTimingHeader())
    } else {
      ClearNodes
    }
  }

  def firstSteps = {
    {
      for {
        user <- currentUser.is
          if ! user.firstSteps.isEmpty
      } yield {
        ClearClearable andThen
        ".first-steps" #> {
          "li" #> user.firstSteps.values.map { firstStep =>
            "a [href]" #> firstStep.href &
            "a *" #> firstStep.description
          }
        }
      }
    } openOr {
      ClearNodes
    }
  }

  val googleAnalyticsId = Props.get("anchortab.analyticsid") openOr ""
  def googleAnalytics(xhtml:NodeSeq) = {
    Unparsed("""
      <script type="text/javascript">

        var _gaq = _gaq || [];
        _gaq.push(['_setAccount', '""" + googleAnalyticsId + """']);
        _gaq.push(['_trackPageview']);

        (function() {
          var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
          ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
          var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
        })();

      </script>
    """)
  }

  def userName(xhtml:NodeSeq) = {
    val nameTransform =
      for {
        session <- userSession.is
        user <- session.user
      } yield {
        "span *" #> user.email
      }

    nameTransform.map(_.apply(xhtml)) openOr NodeSeq.Empty
  }

  def userGravatar(xhtml:NodeSeq) = {
    import java.security.MessageDigest

    def md5(s: String) = {
      MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X".format(_)).mkString.toLowerCase
    }

    def gravatarUriFor(email:String) = {
      val hashedEmail = md5(email)
      "https://www.gravatar.com/avatar/" + hashedEmail + ".jpg?s=60"
    }

    val gravatarTransform =
      for {
        session <- userSession.is
        user <- session.user
      } yield {
        "img [src]" #> gravatarUriFor(user.email)
      }

    gravatarTransform.map(_.apply(xhtml)) openOr NodeSeq.Empty
  }

  def highlightActiveNavigation(xhtml:NodeSeq) = {
    val highlightFunc = {
      S.uri match {
        case "/manager/dashboard" =>
          Some(".dashboard [class+]" #> "selected")

        case s if s.startsWith("/manager/tab") =>
          Some(".manage-tabs [class+]" #> "selected")

        case s if s.startsWith("/manager/subscribers") =>
          Some(".subscribers [class+]" #> "selected")

        case "/manager/analytics" =>
          Some(".analytics [class+]" #> "selected")

        case "/manager/account" =>
          Some(".profile [class+]" #> "selected")

        case "/manager/subscription" =>
          Some(".subscription [class+]" #> "selected")

        case "/manager/services" =>
          Some(".connected-services [class+]" #> "selected")

        case "/manager/affiliate" =>
          Some(".affiliate [class+]" #> "selected")

        case s if s.startsWith("/admin/user") =>
          Some(".users [class+]" #> "selected")

        case s if s.startsWith("/admin/plan") =>
          Some(".plans [class+]" #> "selected")

        case s if s.startsWith("/admin/miracle") =>
          Some(".miracles [class+]" #> "selected")

        case s if s.startsWith("/admin/affiliate-report") =>
          Some(".affiliate-report [class+]" #> "selected")

        case _ => None
      }
    }

    highlightFunc.map(_.apply(xhtml)) getOrElse xhtml
  }

  def ieConditional(xhtml:NodeSeq) = {
    val ieVersion = S.attr("version") openOr "IE"
    Unparsed("<!--[if " + ieVersion + "]>") ++ xhtml ++ Unparsed("<![endif]-->")
  }

  def jquery(xhtml:NodeSeq) = {
    val jQueryUrl = Props.get("jquery.url")

    jQueryUrl.map { url =>
      ("script [src]" #> url).apply(xhtml)
    } openOr {
      xhtml
    }
  }

  def setPageTitle(xhtml:NodeSeq) = {
    currentPageTitle(Full(xhtml.text))
    NodeSeq.Empty
  }

  def pageTitle(xhtml:NodeSeq) = {
    val currentTitle = xhtml.text
    val newTitle = {
      currentPageTitle.is.map { pageTitle =>
        pageTitle + " | " + currentTitle
      }
    } openOr {
      currentTitle
    }

    ("title *" #> newTitle).apply(xhtml)
  }
}
