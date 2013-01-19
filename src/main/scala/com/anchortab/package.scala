package com

import net.liftweb.http.js._
  import JsCmds._
  import JE._
import net.liftweb.http.SHtml._

package object anchortab {
  def onEventIf(question:String, fn:(String)=>JsCmd) : (String, JsExp) = {
    val guidExp = onEvent(fn)

    (guidExp.guid, JsRaw(Confirm(question, guidExp.exp).toJsCmd))
  }
}
