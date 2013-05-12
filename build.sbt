import AssemblyKeys._

assemblySettings

name := "Anchor Tab"

version := "1.1.0-SNAPSHOT"

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
  val liftVersion = "2.5-RC5"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "net.liftweb"       %% "lift-mongodb"       % liftVersion        % "compile",
    "net.liftmodules"   %% "lift-jquery-module_2.5" % "2.3",
    "javax.servlet"     %  "servlet-api"        % "2.5" % "provided",
    "org.eclipse.jetty" % "jetty-webapp"        % "8.1.10.v20130312"  % "compile,container",
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "provided,compile,container" artifacts Artifact("javax.servlet", "jar", "jar"),
    "ch.qos.logback"    % "logback-classic"     % "1.0.11",
    "joda-time"         % "joda-time"           % "2.2",
    "org.joda"          % "joda-convert"        % "1.2",
    "org.mindrot"       % "jbcrypt"             % "0.3m",
    "net.databinder.dispatch" %% "dispatch-core" % "0.9.5",
    "net.databinder.dispatch" %% "dispatch-lift-json" % "0.9.5" intransitive(),
    "com.stripe"        %% "stripe-scala"       % "1.1.2",
    "com.ecwid"         % "ecwid-mailchimp"     % "1.3.0.5",
    "org.scalatest"     %% "scalatest"          % "2.0.M5b" % "test->default",
    "org.seleniumhq.selenium" % "selenium-java" % "2.29.1"  % "test->default",
    "com.newrelic.agent.java" % "newrelic-api"  % "2.17.1",
    "me.frmr.newrelic"  %% "lift-newrelic"      % "1.0.0"
  )
}

parallelExecution in Test := false

mainClass in assembly := Some("bootstrap.Start")

test in assembly := {}

jarName in assembly := "anchortab.jar"

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "about.html" => MergeStrategy.first
    case x => old(x)
  }
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
