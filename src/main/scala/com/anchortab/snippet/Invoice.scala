package com.anchortab.snippet

import net.liftweb._
  import sitemap._
    import Loc._
  import common._
  import http._
    import SHtml._
  import util._
    import Helpers._

import com.stripe

import com.anchortab.model._
import com.anchortab.util._
import org.joda.time._

object Invoice {
  val menu = Menu.param[String](
    "Invoice",
    "Invoice",
    s => Full(s),
    s => s
  ) / "manager" / "invoice" / * >>
  TemplateBox(() => Templates("manager" :: "invoice" :: Nil))
}
class Invoice(invoiceId: String) extends StripeInvoiceRendering {
  def render = {
    {
      for {
        userSession <- userSession.is
        user <- User.find(userSession.userId)
        customerId <- user.stripeCustomerId
        invoice <- tryo(stripe.Invoice.retrieve(invoiceId))
         if invoice.customer == customerId
      } yield {
        renderInvoice(user, invoice)
      }
    } openOr {
      "*" #> ClearNodes
    }
  }
}
