import sbt._
import Keys._

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
        )

  object versions {
    val dispatch = "0.9.4"
    val httpclient = "4.2.1"
    val jetty = "8.0.4.v20111024"
    val scalaz = "7.0.0-M4"
  }

  lazy val dependencies =
    List( "commons-codec" % "commons-codec" % "1.6" // for dispatch"
        , "net.databinder.dispatch" %% "dispatch-core" % versions.dispatch intransitive()
        , "org.apache.httpcomponents" % "httpcore" % versions.httpclient intransitive()
        , "org.apache.httpcomponents" % "httpclient" % versions.httpclient intransitive()
        , "org.eclipse.jetty" % "jetty-webapp" % versions.jetty
        , "org.eclipse.jetty" % "jetty-plus" % versions.jetty
        , "org.scalaz" %% "scalaz-core" % versions.scalaz
        , "org.scalaz" %% "scalaz-effect" % versions.scalaz
        )

  lazy val deploySettings: Seq[Setting[_]] =
    heroic.Plugin.heroicSettings ++
    List( mainClass := Some("com.atlassian.ecosystem.marvin.Main")
//         , heroJavaOptions := 
        )

}
