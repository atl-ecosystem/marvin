import sbt._
import Keys._

import com.typesafe.sbt.SbtStartScript

object MarvinBuild extends Build {
  lazy val root = 
    Project( id = "marvin"
           , base = file(".")
           , settings = Defaults.defaultSettings ++ projectSettings ++ deploySettings
           ).settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*)

  lazy val projectSettings: List[Setting[_]] = 
    List( organization := "com.atlassian.ecosystem"
        , name := "marvin"
        , scalaVersion := "2.9.2"
        , scalacOptions ++= List("-unchecked", "-deprecation", "-Ydependent-method-types", "-Ywarn-value-discard")
        , libraryDependencies := dependencies
        , resolvers ++= resolvers_
        )

  object versions {
    val dispatch = "0.9.4"
    val httpclient = "4.2.1"
    val jetty = "8.0.4.v20111024"
    val scalaz = "7.0.0-M4"
    val argonaut = "4.0"
  }

  lazy val dependencies =
    List( "com.ephox" %% "argonaut" % versions.argonaut intransitive()
        , "commons-codec" % "commons-codec" % "1.6" // for dispatch"
        , "commons-fileupload" % "commons-fileupload" % "1.2.2"
        , "commons-io" % "commons-io" % "1.3.2"
        , "net.databinder.dispatch" %% "dispatch-core" % versions.dispatch intransitive()
        , "org.apache.httpcomponents" % "httpcore" % versions.httpclient intransitive()
        , "org.apache.httpcomponents" % "httpclient" % versions.httpclient intransitive()
        , "org.eclipse.jetty" % "jetty-webapp" % versions.jetty
        , "org.eclipse.jetty" % "jetty-plus" % versions.jetty
        , "org.scalaz" %% "scalaz-core" % versions.scalaz
        , "org.scalaz" %% "scalaz-effect" % versions.scalaz
        )

  lazy val resolvers_ = 
    List( "mth.io snapshots"  at "http://repo.mth.io/snapshots"
        , "mth.io releases"  at "http://repo.mth.io/releases"
        )

  lazy val deploySettings: Seq[Setting[_]] =
    SbtStartScript.startScriptForClassesSettings ++
    List( mainClass := Some("com.atlassian.ecosystem.marvin.Main")
        )

}
