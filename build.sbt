import AssemblyKeys._

assemblySettings

seq(webSettings :_*)

seq(resourceManagementSettings :_*)

name := "Anchor Tab"

version := "1.3.3-SNAPSHOT"

organization := "com.anchortab"

scalaVersion := "2.10.3"

awsAccessKey := Some("AKIAIL7PSSMYIX5IEOIA")

awsSecretKey := Some("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")

awsS3Bucket := Some("assets.anchortab.com")

resolvers ++= Seq("snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
                "releases"        at "http://oss.sonatype.org/content/repositories/releases",
                "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
                )

net.virtualvoid.sbt.graph.Plugin.graphSettings

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:postfixOps")

port in container.Configuration := 8081

libraryDependencies ++= {
  val liftVersion = "2.6-M3"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "net.liftweb"       %% "lift-testkit"       % liftVersion        % "test",
    "net.liftweb"       %% "lift-mongodb"       % liftVersion        % "compile",
    "net.liftmodules"   %% "lift-jquery-module_2.5" % "2.3",
    "javax.servlet"     %  "servlet-api"        % "2.5" % "provided",
    "org.eclipse.jetty" % "jetty-webapp"        % "9.1.4.v20140401"  % "compile,container",
    "ch.qos.logback"    % "logback-classic"     % "1.0.13",
    "joda-time"         % "joda-time"           % "2.3",
    "org.joda"          % "joda-convert"        % "1.5",
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
    "net.databinder.dispatch" %% "dispatch-lift-json" % "0.11.0",
    "com.stripe"        %% "stripe-scala"       % "1.1.2",
    "com.ecwid"         % "ecwid-mailchimp"     % "1.3.0.7",
    "org.scalatest"     %% "scalatest"          % "2.0" % "it,test",
    "org.seleniumhq.selenium" % "selenium-java" % "2.37.0" % "it",
    "org.apache.httpcomponents" % "httpclient"  % "4.3.1"  % "it",
    "com.newrelic.agent.java" % "newrelic-api"  % "3.1.0",
    "me.frmr.newrelic"  %% "lift-newrelic"      % "1.1.0",
    "com.createsend"    % "createsend-java"     % "5.0.0"
  )
}

parallelExecution in Test := false

parallelExecution in IntegrationTest := false

test in IntegrationTest <<= (test in IntegrationTest) dependsOn(com.earldouglas.xsbtwebplugin.PluginKeys.start in com.earldouglas.xsbtwebplugin.WebPlugin.container.Configuration)

mainClass in assembly := Some("bootstrap.Start")

test in assembly := {}

jarName in assembly := "anchortab.jar"

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  cp filter {_.data.getName == "stax-api-1.0.1.jar"}
}

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
