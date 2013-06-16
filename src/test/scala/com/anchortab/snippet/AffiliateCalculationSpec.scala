package com.anchortab.snippet

import org.scalatest._

class AffiliateCalculationSpec extends FunSpec with ShouldMatchers {
  describe("Total as of Target Month Calculator") {
    it("+1 for each paid sub w/ code; start before target month; no end date") (pending)

    it("+1 for each paid sub w/ code; start before target month; end date after target mo") (pending)

    it("+1 for each paid sub w/ code; start in target mo; without end date") (pending)

    it("+1 for each paid sub w/ code; start in target mo; end date 20 days after start") (pending)
  }

  describe("New Users for Target Month Calculator") {
    it("+1 for first sub by new user w/ code; start in target mo; no end date") (pending)

    it("+1 for first sub by new user w/ code; start in target mo; end date > 1 mo after start") (pending)
  }
}
