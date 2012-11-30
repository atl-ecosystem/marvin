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
        , scalaVersion := versions.scala
        , scalacOptions ++= List("-unchecked", "-deprecation", "-Ydependent-method-types", "-Ywarn-value-discard")
        , libraryDependencies := dependencies
        , resolvers ++= resolvers_
        )

  object versions {
    val dispatch = "0.9.4"
    val httpclient = "4.2.1"
    val jetty = "8.0.4.v20111024"
    val scala = "2.9.2"
    val scalaz = "7.0.0-M1"
    val argonaut = "4.0"
  }

  lazy val dependencies =
    List( "com.ephox" %% "argonaut" % versions.argonaut intransitive()
        , "org.scala-lang" % "scala-compiler" % versions.scala
        , "org.slf4j" % "slf4j-simple" % "1.6.2"
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
