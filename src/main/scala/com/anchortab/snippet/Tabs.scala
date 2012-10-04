package com.anchortab.snippet

import scala.xml.NodeSeq

import net.liftweb._
  import http._
    import SHtml._
  import common._
  import util._
    import Helpers._

import com.anchortab.model.Tab

import org.bson.types.ObjectId

object requestTabId extends RequestVar[Box[String]](Empty)

object Tabs {
  def requestTab = requestTabId.is.flatMap(id => Tab.find(new ObjectId(id)))

  def tabForm = {
    var tabName = requestTab.map(_.name) openOr ""
    var appearanceDelay = requestTab.map(_.appearance.delay.toString) openOr ""
    var font = requestTab.map(_.appearance.font) openOr ""
    var colorScheme = requestTab.map(_.appearance.colorScheme) openOr ""
    var customText = requestTab.map(_.appearance.customText) openOr ""

    def submit = {
      // TODO
    }

    val bind =
      "#tab-name" #> text(tabName, tabName = _) &
      "#appearance-delay" #> selectObj[Tab.AppearanceDelayOptions.Value](
        Tab.AppearanceDelayOptions.values.toList.map(v => (v,v.toString)),
        tryo(Tab.AppearanceDelayOptions.withName(appearanceDelay)),
        selected => appearanceDelay = selected.toString
      ) &
      "#font" #> selectObj[Tab.FontOptions.Value](
        Tab.FontOptions.values.toList.map(v => (v,v.toString)),
        tryo(Tab.FontOptions.withName(font)),
        selected => font = selected.toString
      ) &
      "#color-scheme" #> selectObj[Tab.ColorSchemeOptions.Value](
        Tab.ColorSchemeOptions.values.toList.map(v => (v,v.toString)),
        tryo(Tab.ColorSchemeOptions.withName(colorScheme)),
        selected => colorScheme = selected.toString
      )

    "form" #> { ns:NodeSeq =>
      ajaxForm(bind(ns))
    }
  }
}
