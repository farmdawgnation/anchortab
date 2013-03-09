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

import net.liftmodules.JQueryModule

import com.stripe


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
    LiftRules.statelessRewrite.append(Invites.statelessRewrite)
    LiftRules.statelessRewrite.append(Tabs.statelessRewrite)
    LiftRules.statelessRewrite.append(Admin.statelessRewrite)
    LiftRules.statelessRewrite.append(Authentication.statelessRewrite)

    // Add Snippet handlers
    LiftRules.snippets.append(Bundles.snippetHandlers)
    LiftRules.snippets.append(Util.snippetHandlers)
    LiftRules.snippets.append(Authentication.snippetHandlers)
    LiftRules.snippets.append(Accounts.snippetHandlers)
    LiftRules.snippets.append(Admin.snippetHandlers)
    LiftRules.snippets.append(Invites.snippetHandlers)
    LiftRules.snippets.append(Tabs.snippetHandlers)
    LiftRules.snippets.append(Subscription.snippetHandlers)

    //Init the jQuery module, see http://liftweb.net/jquery for more information.
    LiftRules.jsArtifacts = JQueryArtifacts
    JQueryModule.InitParam.JQuery=JQueryModule.JQuery172
    JQueryModule.init()

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Force URLs processing sensitive data to SSL in production.
    if (Props.productionMode) {
      val sslProtectedPaths = "/(lost-sticky-note|amnesia|register|accept-invite|admin|manager|session)".r
      LiftRules.earlyResponse.append { req =>
        val uriAndQueryString = req.uri + (req.request.queryString.map(s => "?"+s) openOr "")

        if (! req.header("X-Secure-Request").isDefined && sslProtectedPaths.findFirstMatchIn(req.uri).isDefined) {
          val uri = "https://%s%s".format(req.request.serverName, uriAndQueryString)
          Full(PermRedirectResponse(uri, req, req.cookies: _*))
        } else {
          Empty
        }
      }
    }

    // If we receive a jQuery JSONP call and we don't call the callback, add in a bogus
    // call to the callback function.
    LiftRules.responseTransformers.append { response =>
      response.toResponse match {
        case resp @ InMemoryResponse(data, headers, _, _) =>
          {
            for(callback <- S.param("callback")) yield {
              val dataStr = new String(data)

              if (! dataStr.contains(callback)) {
                val updatedData = dataStr + "\n" + callback + "();"
                val updatedHeaders = headers.collect {
                  case (headerName, value) if headerName == "Content-Length" =>
                    (headerName, updatedData.length.toString)

                  case otherHeader => otherHeader
                }

                resp.copy(data = updatedData.getBytes("UTF-8"), headers = updatedHeaders)
              } else {
                resp
              }
            }
          } openOr {
            resp
          }

        case _ =>
          response
      }
    }

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    // Schedule quota reset
    QuotasActor ! ScheduleQuotaReset

    // Set Stripe API key
    stripe.apiKey = "sk_test_eIk3iZ4csTxHtFmmh86M0E1n"
  }
}
