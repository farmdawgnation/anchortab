import AssemblyKeys._

assemblySettings

name := "Anchor Tab"

version := "0.1.0"

organization := "com.anchortab"

scalaVersion := "2.9.2"

awsAccessKey := "AKIAIIUM6RAXWVRN4NNQ"

awsSecretKey := "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"

awsS3Bucket := "assets.anchortab.com"

resolvers ++= Seq("snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
                "releases"        at "http://oss.sonatype.org/content/repositories/releases",
                "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
                )

seq(com.github.siasia.WebPlugin.webSettings :_*)

seq(resourceManagementSettings :_*)

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= {
  val liftVersion = "2.5-M4"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "net.liftweb"       %% "lift-mongodb"       % liftVersion        % "compile",
    "net.liftmodules"   %% "lift-jquery-module" % (liftVersion + "-2.1"),
    "javax.servlet"     %  "servlet-api"        % "2.5" % "provided",
    "org.eclipse.jetty" % "jetty-webapp"        % "8.1.7.v20120910"  % "compile,container",
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "provided,compile,container" artifacts Artifact("javax.servlet", "jar", "jar"),
    "ch.qos.logback"    % "logback-classic"     % "1.0.6",
    "joda-time"         % "joda-time"           % "2.1",
    "org.joda"          % "joda-convert"        % "1.2",
    "org.mindrot"       % "jbcrypt"             % "0.3m",
    "me.frmr.wepay-scala" %% "wepay-scala"       % "0.9.1-SNAPSHOT",
    "com.ecwid"         % "ecwid-mailchimp"     % "1.3.0.5-SNAPSHOT"
  )
}

mainClass in assembly := Some("bootstrap.Start")

jarName in assembly := "anchortab.jar"

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "about.html" => MergeStrategy.first
    case x => old(x)
  }
}

excludedJars in assembly <<= (fullClasspath in assembly) map { cp => 
  cp filter {_.data.getName == "lift-json_2.9.1-2.4.jar"}
}

initialCommands := """
import com.anchortab.model._
import com.anchortab.constantcontact._
import com.anchortab.constantcontact.model._
import net.liftweb.json._
import Extraction._
val boot = new bootstrap.liftweb.Boot
boot.boot
"""
