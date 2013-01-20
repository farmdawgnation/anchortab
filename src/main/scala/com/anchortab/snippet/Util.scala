package com.anchortab.snippet

import scala.xml._

import net.liftweb._
  import common._
  import http._
    import LiftRules._
    import rest._
    import js._
      import JE._
      import JsExp._
  import util._
    import Helpers._
  import json._
    import JsonDSL._
    import Extraction._
  import mongodb._
    import BsonDSL._

class SimpleAnchorTabEvent(eventName:String) extends JsCmd {
  import Serialization._

  implicit def typeHints = Serialization.formats(NoTypeHints)

  def toJsCmd = {
    Call("anchortab.event", eventName, decompose(this)).cmd.toJsCmd
  }
}
class AnchorTabEvent(eventName:String, parameters:JObject) extends JsCmd {
  def toJsCmd = {
    Call("anchortab.event", eventName, parameters).cmd.toJsCmd
  }
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
    case "uservoice" :: Nil => uservoice _
  }

  def uservoice(xhtml: NodeSeq) = {
    Unparsed("""
      <script type="text/javascript">
        var uvOptions = {};
        (function() {
          var uv = document.createElement('script'); uv.type = 'text/javascript'; uv.async = true;
          uv.src = ('https:' == document.location.protocol ? 'https://' : 'http://') + 'widget.uservoice.com/8GBnAHDKflWRMP1tBP9y1A.js';
          var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(uv, s);
        })();
      </script>
    """)
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

        case s if s.startsWith("/admin/user") =>
          Some(".users [class+]" #> "selected")

        case s if s.startsWith("/admin/plan") =>
          Some(".plans [class+]" #> "selected")

        case s if s.startsWith("/admin/invite") =>
          Some(".invites [class+]" #> "selected")

        case s if s.startsWith("/admin/miracle") =>
          Some(".miracles [class+]" #> "selected")

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
