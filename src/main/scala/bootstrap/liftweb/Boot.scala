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

    // Early stateful and stateless mehtods.
    LiftRules.earlyInStateful.prepend(Authentication.earlyInStateful)
    LiftRules.earlyInStateless.prepend(Authentication.earlyInStateless)

    // Add API to dispatch
    LiftRules.statelessDispatch.append(Api)

    // Authentication dispatch.
    LiftRules.dispatch.append(Authentication.dispatch)
    LiftRules.dispatch.append(OAuth.dispatch)

    // Add API to stateless rewrites
    LiftRules.statelessRewrite.append(Api.statelessRewrite)
    LiftRules.statelessRewrite.append(Tabs.statelessRewrite)
    LiftRules.statelessRewrite.append(Admin.statelessRewrite)

    // Add Snippet handlers
    LiftRules.snippets.append(Bundles.snippetHandlers)
    LiftRules.snippets.append(Util.snippetHandlers)
    LiftRules.snippets.append(Authentication.snippetHandlers)
    LiftRules.snippets.append(Accounts.snippetHandlers)
    LiftRules.snippets.append(Admin.snippetHandlers)

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
