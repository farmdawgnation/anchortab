package com.anchortab.snippet

import org.scalatest._

import com.anchortab.util._

import net.liftweb.json._
  import JsonDSL._

class AnchorTabEventSpec extends FunSuite {
  test("AnchorTabEvent is serialized to a JsCmd") {
    case class TestAnchorTabEvent(testParam1: Int, testParam2: String) extends
      AnchorTabEvent("test-event", (("testParam1" -> testParam1) ~ ("testParam2" -> testParam2)))

    val testEvent = TestAnchorTabEvent(500, "bacon")
    val testEventJsCmd = testEvent.toJsCmd

    val expectedJsCmd = "anchortabSite.event(\"test-event\",{\"testParam1\":500,\"testParam2\":\"bacon\"});"

    assert(testEventJsCmd == expectedJsCmd)
  }

  test("SimpleAnchorTabEvent is serialized to a JsCmd") {
    case class TestAnchorTabEvent(testParam1: Int, testParam2: String) extends SimpleAnchorTabEvent("test-event")

    val testEvent = TestAnchorTabEvent(500, "bacon")
    val testEventJsCmd = testEvent.toJsCmd

    val expectedJsCmd = "anchortabSite.event(\"test-event\",{\"$outer\":{\"$outer\":{}},\"testParam1\":500,\"testParam2\":\"bacon\"});"

    assert(testEventJsCmd == expectedJsCmd)
  }
}

class UtilSpec extends FunSuite {
}
