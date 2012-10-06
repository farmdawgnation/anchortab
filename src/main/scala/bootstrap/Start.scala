package bootstrap

import java.io.File
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
import net.liftweb.util.Props

object Start {

  def main(args: Array[String]): Unit = {
    
    /* choose different port for each of your webapps deployed on single server
     * you may use it in nginx proxy-pass directive, to target virtual hosts */
    val port = Props.getInt("port", 8090)

    val server = new Server(port)
    val domain = Start.getClass.getProtectionDomain
    val location = domain.getCodeSource.getLocation

    val webapp = new WebAppContext
    webapp.setServer(server)
    webapp.setContextPath("/")
    /* use resource base to avoid mixing your webapp files on the top level
     * of the executable jar, with all the included libraries etc
     * here I used webapp dir as it matches target dir of package-war task and makes
     * merging of webapp dir with output of assembly easier */
    webapp.setResourceBase("webapp")
    /* also include webapp dir in path to web.xml */
    webapp.setDescriptor(location.toExternalForm() + "/webapp/WEB-INF/web.xml")

    webapp.setServer(server)
    webapp.setWar(location.toExternalForm())

    // (Optional) Set the directory the war will extract to.
    // If not set, java.io.tmpdir will be used, which can cause problems
    // if the temp directory gets cleaned periodically.
    // Your build scripts should remove this directory between deployments
    //webapp.setTempDirectory(new File("./extract"))

    server.setHandler(webapp)
    server.start
    server.join
  }
}
