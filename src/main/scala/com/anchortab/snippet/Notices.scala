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
  private trait Notice {
    def noticeType: String
    def message: String
  }
  private case class TransientNotice(noticeType: String, message: String) extends Notice
  private case class StickyNotice(name: String, noticeType: String, message: String) extends Notice

  private object currentNotices extends SessionVar[List[Notice]](Nil)
  private object stickyNotices extends SessionVar[List[StickyNotice]](Nil)

  private def pushNotice(noticeType: String, message: String, stickyName: Option[String]) {
    currentNotices(currentNotices.is :+ TransientNotice(noticeType, message))

    stickyName.foreach { stickyNoticeName =>
      stickyNotices(stickyNotices.is :+ StickyNotice(stickyNoticeName, noticeType, message))
    }
  }

  def notice(message: String, stickyName: Option[String] = None) {
    pushNotice(NoticeTypes.notice, message, stickyName)
  }

  def warning(message: String, stickyName: Option[String] = None) {
    pushNotice(NoticeTypes.warning, message, stickyName)
  }

  def error(message: String, stickyName: Option[String] = None) {
    pushNotice(NoticeTypes.error, message, stickyName)
  }

  def removeStickyNotice(stickyName: String) {
    stickyNotices(stickyNotices.is.filterNot(_.name == stickyName))
  }

  def render = {
    val notices: Map[String, List[Notice]] = currentNotices.is.groupBy(_.noticeType)
    currentNotices(stickyNotices)

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
