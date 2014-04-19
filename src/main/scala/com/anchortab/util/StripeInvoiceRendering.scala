package com.anchortab.util

import net.liftweb._
  import util._
    import Helpers._

import com.stripe._

import com.anchortab.model.User

trait StripeInvoiceRendering {
  def renderInvoice(user: User, invoice: Invoice) = {
    ".customer-email *" #> user.email &
    ".paid-status *" #> (invoice.paid ? "Yes" | "No") &
    ".invoice-date *" #> StripeNumber(invoice.date).asDateTime.toString(DATE_FORMAT) &
    ".invoice-total *" #> StripeNumber(invoice.total).asDollarsAndCentsString &
    ".line-item" #> invoice.lines.data.map { invoiceLine =>
      ".line-item-description *" #> (invoiceLine.description orElse invoiceLine.plan.map(_.name)) &
      ".period *" #> (invoiceLine.plan.isDefined ? PassThru | ClearNodes) andThen
      ".period-start *" #> StripeNumber(invoiceLine.period.start).asDateTime.toString(DATE_FORMAT) &
      ".period-end *" #> StripeNumber(invoiceLine.period.end).asDateTime.toString(DATE_FORMAT) &
      ".amount *" #> StripeNumber(invoiceLine.amount).asDollarsAndCentsString
    } &
    ".payment-line-item" #> invoice.charge.flatMap(chargeId => tryo(Charge.retrieve(chargeId))).map { charge =>
      ".card-type *" #> charge.card.`type` &
      ".last-four *" #> charge.card.last4 &
      ".amount *" #> ("-" + StripeNumber(charge.amount).asDollarsAndCentsString)
    }
  }
}
