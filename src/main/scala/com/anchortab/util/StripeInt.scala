package com.anchortab.util

import org.joda.time._

case class StripeInt(input: Int) {
  lazy val asDateTime = new DateTime(input * 1000)
  lazy val asDollarsAndCents = input / 100d
  lazy val asDollarsAndCentsString = ("$%.2f" format asDollarsAndCents)
}
