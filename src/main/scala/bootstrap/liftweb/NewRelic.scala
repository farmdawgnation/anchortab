package bootstrap.liftweb

import net.liftweb._
  import util._
    import Helpers._
  import common._
  import http._
  import js.jquery.JQueryArtifacts
  import sitemap._
  import Loc._
  import mongodb._

import com.newrelic.api.agent._

object NewRelicBootstrap {
  protected def nameTransaction(req: Box[Req]) {
    for (req <- req) {
      LiftRules.siteMap.flatMap(_.findLoc(req)).map { matchedLoc =>
        val transactionName = "/" + matchedLoc.link.uriList.mkString("/")
        NewRelic.setTransactionName("SiteMap", transactionName)
      } openOr {
        if (req.uri.startsWith("/comet_request")) {
          NewRelic.setTransactionName("AJAX", "/comet_request")
        } else if (req.uri.startsWith("/ajax_request")) {
          NewRelic.setTransactionName("AJAX", "/ajax_request")
        }
      }
    }
  }

  def newRelicTransactionNaming = {
    LiftRules.earlyInStateful.append(nameTransaction)
    LiftRules.earlyInStateless.append(nameTransaction)
  }
}
