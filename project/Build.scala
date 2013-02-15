import sbt._
import Keys._

object MarvinBuild extends Build {
  lazy val root = 
    Project( id = "marvin"
           , base = file(".")
           , settings = Defaults.defaultSettings ++ projectSettings
           ).settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*)

  lazy val projectSettings: List[Setting[_]] = 
    List( organization := "purefn"
        , name := "marvin"
        , scalaVersion := versions.scala
        , scalacOptions ++= List("-unchecked", "-deprecation", "-Ydependent-method-types", "-Ywarn-value-discard")
        , libraryDependencies ++= dependencies
        , initialCommands in console := consoleInit
        )

  object versions {
    val scala = "2.9.2"
    val scalaz = "7.0.0-M2"
    val smack = "3.2.1"
  }

  lazy val dependencies =
    List( "org.scala-lang" % "scala-compiler" % versions.scala
        , "org.scalaz" %% "scalaz-concurrent" % versions.scalaz
        , "org.igniterealtime.smack" % "smack" % versions.smack
        , "org.igniterealtime.smack" % "smackx" % versions.smack
        )

  lazy val consoleInit = """
import scalaz._, Scalaz._
import purefn.marvin._
"""

}
