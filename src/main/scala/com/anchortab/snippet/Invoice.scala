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
class Invoice(invoiceId: String) {
  def render = {
    {
      for {
        userSession <- userSession.is
        user <- User.find(userSession.userId)
        customerId <- user.stripeCustomerId
        invoice <- tryo(stripe.Invoice.retrieve(invoiceId))
         if invoice.customer == customerId
      } yield {
        ".customer-email *" #> user.email &
        ".paid-status *" #> (invoice.paid ? "Yes" | "No") &
        ".invoice-date *" #> new DateTime(invoice.date * 1000).toString("YYYY-MM-dd") &
        ".invoice-total *" #> ("$%.2f" format invoice.total / 100d) &
        ".line-item" #> invoice.lines.data.map { invoiceLine =>
          ".line-item-description *" #> (invoiceLine.description orElse invoiceLine.plan.map(_.name)) &
          ".period *" #> (invoiceLine.plan.isDefined ? PassThru | ClearNodes) andThen
          ".period-start *" #> new DateTime(invoiceLine.period.start * 1000).toString("YYYY-MM-dd") &
          ".period-end *" #> new DateTime(invoiceLine.period.end * 1000).toString("YYYY-MM-dd") &
          ".amount *" #> ("$%.2f" format invoiceLine.amount / 100d)
        } &
        ".payment-line-item" #> invoice.charge.flatMap(chargeId => tryo(stripe.Charge.retrieve(chargeId))).map { charge =>
          ".card-type *" #> charge.card.`type` &
          ".last-four *" #> charge.card.last4 &
          ".amount *" #> ("-$%.2f" format charge.amount / 100d)
        }
      }
    } openOr {
      "*" #> ClearNodes
    }
  }
}
