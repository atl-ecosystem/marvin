package com.atlassian.ecosystem.marvin

import scalaz.{Source ⇒ _, _}
import effect._
import IO._
import syntax.monad._
import syntax.validation._
import syntax.std.option._
import syntax.std.string._

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.handler.{ContextHandler, ResourceHandler, HandlerList}
import org.eclipse.jetty.servlet.{ServletHolder, ServletContextHandler}


case class Config
  ( port: Int
  , hipchatToken: String
  , jiraBase: String
  , linkIssueKey: String
  , raiseIssueKey: String
  , scalaReplKey: String
  )

object Main {
  def main(args: Array[String]): Unit = mainIO(ImmutableArray.fromArray(args)) unsafePerformIO

  def mainIO(args: ImmutableArray[String]): IO[Unit] = 
    getConfig >>= createServer >>= start
  
  type ConfigParseResult[A] = ValidationNEL[String, A]
  def getConfig: IO[Config] =
    Apply[IO].compose[ConfigParseResult].map6( getPort
                                             , getReqEnv("HIPCHAT_TOKEN")
                                             , getReqEnv("JIRA_BASE")
                                             , getReqEnv("LINK_ISSUE_KEY")
                                             , getReqEnv("RAISE_ISSUE_KEY")
                                             , getReqEnv("SCALA_REPL_KEY")
                                             )(Config.apply) >>= 
      (_.fold(success = _.point[IO], failure = errs ⇒ throwIO(new RuntimeException(errs.list.mkString))))

  def getEnv(name: String): IO[Option[String]] = IO(Option(System.getenv(name)))

  def getReqEnv(name: String): IO[ConfigParseResult[String]] =
    getEnv(name) map (_.fold(some = _.successNel, none = "Missing env var '%s'".format(name).failNel))

  def getPort: IO[ConfigParseResult[Int]] =
    getEnv("PORT") map (s ⇒
      s.getOrElse("8080").parseInt.fold(success = _.successNel, failure = _ ⇒ "Invalid port '%s'".format(s).failNel))

  def createServer(config: Config): IO[Server] = IO {
    val servlets = new ServletContextHandler
    servlets.setContextPath("/")
    servlets.addServlet(new ServletHolder(LinkIssueServlet(config)), "/link-issue")
    servlets.addServlet(new ServletHolder(RaiseIssueServlet(config)), "/raise-issue")
    servlets.addServlet(new ServletHolder(ScalaServlet(config)), "/scala-repl")

    val handlers = new HandlerList
    handlers.setHandlers(Array(servlets))

    val s = new Server(config.port)
    s.setHandler(handlers)
    s
  }

  def start(s: Server): IO[Unit] = IO {
    s.start()
    s.join()
  }
}

