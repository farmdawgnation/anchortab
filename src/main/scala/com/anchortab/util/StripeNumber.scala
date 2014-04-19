package com.anchortab.util

import org.joda.time._

sealed trait StripeNumber
object StripeNumber {
  def apply(input: Int) = StripeInt(input)
  def apply(input: Long) = StripeLong(input)
}

case class StripeInt(input: Int) extends StripeNumber {
  lazy val asDollarsAndCents = input / 100d
  lazy val asDollarsAndCentsString = ("$%.2f" format asDollarsAndCents)
}

case class StripeLong(input: Long) extends StripeNumber {
  lazy val asDateTime = new DateTime(input * 1000)
}
