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

object RequestHandling {
  def setupHtml5 = {
    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))
  }

  def setupPackages = {
    LiftRules.addToPackages("com.anchortab")
  }

  def setupEarlyHandling = {
    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Set up handling for Authentication
    LiftRules.earlyInStateful.prepend(Authentication.earlyInStateful)
    LiftRules.earlyInStateless.prepend(Authentication.earlyInStateless)

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
  }

  def setupJsonpResponseTransformer = {
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
  }

  def setupDispatch = {
    // Add API to dispatch
    LiftRules.statelessDispatch.append(Api)

    // Add stripe hooks to dispatch
    LiftRules.statelessDispatch.append(StripeHook)

    // Authentication dispatch.
    LiftRules.dispatch.append(Authentication.dispatch)
    LiftRules.dispatch.append(OAuth.dispatch)
  }

  def setupRewrites = {
    LiftRules.statelessRewrite.append(Api.statelessRewrite)
  }

  def setupSnippetHandlers = {
    LiftRules.snippets.append(Bundles.snippetHandlers)
    LiftRules.snippets.append(Util.snippetHandlers)
    LiftRules.snippets.append(Authentication.snippetHandlers)
    LiftRules.snippets.append(Accounts.snippetHandlers)
    LiftRules.snippets.append(Admin.snippetHandlers)
    LiftRules.snippets.append(Invites.snippetHandlers)
    LiftRules.snippets.append(Tabs.snippetHandlers)
    LiftRules.snippets.append(Subscription.snippetHandlers)
    LiftRules.snippets.append(Affiliate.snippetHandlers)
  }

  def setupSiteMap = {
    val developmentMenus = {
      if (Props.devMode)
        (Menu.i("Tab Test") / "tabtest") ::
        Nil
      else
        Nil
    }

    val landingMenus: List[ConvertableToMenu] =
      (Menu.i("Home") / "index") ::
      (Menu.i("About Us") / "about-us") ::
      (Menu.i("Terms of Service") / "terms-of-service") ::
      (Menu.i("Privacy Policy") / "privacy-policy") ::
      Nil

    val menus: List[ConvertableToMenu] =
      developmentMenus ++
      landingMenus ++
      Authentication.menus ++
      Dashboard.menus ++
      Tabs.menus ++
      Accounts.menus ++
      Subscription.menus ++
      Invites.menus ++
      Affiliate.menus ++
      Admin.menus ++
      (ForgotPassword.menu :: Nil)

    LiftRules.setSiteMap(SiteMap(menus: _*))
  }
}
