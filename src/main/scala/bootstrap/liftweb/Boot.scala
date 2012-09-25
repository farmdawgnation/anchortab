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

import net.liftmodules.JQueryModule


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    // Connect to MongoDB
    for {
      hostname <- Props.get("mongodb.host")
      port <- Props.get("mongodb.port").map(_.toInt)
      database <- Props.get("mongodb.database")
    } {
      val mongoHost = MongoHost(hostname, port)
      MongoDB.defineDb(DefaultMongoIdentifier, MongoAddress(mongoHost, database))
    }

    // Specify where to search for our stuff.
    LiftRules.addToPackages("com.anchortab")

    // Add API to dispatch
    LiftRules.statelessDispatch.append(Api)

    // Add Snippet handlers
    LiftRules.snippets.append(Authentication.snippetHandlers)

    //Init the jQuery module, see http://liftweb.net/jquery for more information.
    LiftRules.jsArtifacts = JQueryArtifacts
    JQueryModule.InitParam.JQuery=JQueryModule.JQuery172
    JQueryModule.init()

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))
  }
}
