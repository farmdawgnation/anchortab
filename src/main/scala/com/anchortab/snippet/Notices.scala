package com.anchortab.snippet

import scala.xml.NodeSeq
import scala.math._
import scala.collection.immutable.Stack

import net.liftweb._
  import common._
  import mongodb._
  import http._
  import util._
    import Helpers._
  import json._
    import ext._
    import JsonDSL._
    import Extraction._

object Notices {
  private object NoticeTypes {
    val notice = "notice"
    val warning = "warning"
    val error = "error"
  }
  private case class Notice(noticeType: String, message: String)

  private object currentNotices extends SessionVar[Stack[Notice]](Stack.empty)

  private def pushNotice(noticeType: String, message: String) {
    currentNotices(currentNotices.is.push(Notice(noticeType, message)))
  }

  def notice(message: String) {
    pushNotice(NoticeTypes.notice, message)
  }

  def warning(message: String) {
    pushNotice(NoticeTypes.warning, message)
  }

  def error(message: String) {
    pushNotice(NoticeTypes.error, message)
  }

  def render = {
    val notices: Map[String, List[Notice]] = currentNotices.is.toList.groupBy(_.noticeType)
    currentNotices(Stack.empty)

    val noticeStructure =
      <ul>
        <li class="error">Error Text</li>
        <li class="warning">Warning Text</li>
        <li class="notice">Notice Text</li>
      </ul>

    val noticeRenderer =
      ".error" #> notices.get(NoticeTypes.error).getOrElse(Nil).map { errorNotice =>
        ".error *" #> errorNotice.message
      } &
      ".warning" #> notices.get(NoticeTypes.warning).getOrElse(Nil).map { warningNotice =>
        ".warning *" #> warningNotice.message
      } &
      ".notice" #> notices.get(NoticeTypes.notice).getOrElse(Nil).map { notice =>
        ".notice *" #> notice.message
      }

    noticeRenderer(noticeStructure)
  }
}
