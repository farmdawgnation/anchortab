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

import com.anchortab.snippet._
import com.anchortab.actor._

import me.frmr.newrelic._

import net.liftmodules.JQueryModule

import com.stripe


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    // Connect to MongoDB
    SetupDb.setup

    // Setup various request handling code.
    RequestHandling.setupHtml5
    RequestHandling.setupPackages
    RequestHandling.setupEarlyHandling
    RequestHandling.setupJsonpResponseTransformer
    RequestHandling.setupDispatch
    RequestHandling.setupRewrites
    RequestHandling.setupSnippetHandlers
    RequestHandling.setupSiteMap

    // Set up i18n resources.
    LiftRules.resourceNames = "i18n/tab" :: Nil

    // Bootstrap NewRelic
    NewRelicTransactionNaming.setup

    //Init the jQuery module, see http://liftweb.net/jquery for more information.
    LiftRules.jsArtifacts = JQueryArtifacts
    JQueryModule.InitParam.JQuery=JQueryModule.JQuery172
    JQueryModule.init()

    // Schedule quota reset
    QuotasActor ! ScheduleQuotaReset

    // Schedule neighborhood watch
    NeighborhoodWatchActor ! ScheduleNeighborhoodWatch

    // Set Stripe API key
    stripe.apiKey = Props.get("stripe.apiKey") openOr "sk_test_eIk3iZ4csTxHtFmmh86M0E1n"
    stripe.apiVersion = Some("2013-08-13")
  }
}
