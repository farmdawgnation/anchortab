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
        ".invoice-date *" #> StripeNumber(invoice.date).asDateTime.toString(DATE_FORMAT) &
        ".invoice-total *" #> StripeNumber(invoice.total).asDollarsAndCentsString &
        ".line-item" #> invoice.lines.data.map { invoiceLine =>
          ".line-item-description *" #> (invoiceLine.description orElse invoiceLine.plan.map(_.name)) &
          ".period *" #> (invoiceLine.plan.isDefined ? PassThru | ClearNodes) andThen
          ".period-start *" #> StripeNumber(invoiceLine.period.start).asDateTime.toString(DATE_FORMAT) &
          ".period-end *" #> StripeNumber(invoiceLine.period.end).asDateTime.toString(DATE_FORMAT) &
          ".amount *" #> StripeNumber(invoiceLine.amount).asDollarsAndCentsString
        } &
        ".payment-line-item" #> invoice.charge.flatMap(chargeId => tryo(stripe.Charge.retrieve(chargeId))).map { charge =>
          ".card-type *" #> charge.card.`type` &
          ".last-four *" #> charge.card.last4 &
          ".amount *" #> ("-" + StripeNumber(charge.amount).asDollarsAndCentsString)
        }
      }
    } openOr {
      "*" #> ClearNodes
    }
  }
}
