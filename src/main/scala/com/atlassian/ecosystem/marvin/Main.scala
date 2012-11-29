package com.atlassian.ecosystem.marvin

import scalaz._
import effect._
import IO._
import syntax.monad._
import syntax.std.string._

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.handler.{ContextHandler, ResourceHandler, HandlerList}
import org.eclipse.jetty.servlet.{ServletHolder, ServletContextHandler}

import javax.servlet.http._

object WebApp {
  def main(args: Array[String]): Unit = mainIO(ImmutableArray.fromArray(args)) unsafePerformIO

  def mainIO(args: ImmutableArray[String]): IO[Unit] = 
    getPort >>= createServer >>= start

  def getPort: IO[Int] =
    for {
      s ← IO(Option(System.getenv("PORT")))
      p ← s.getOrElse("8080").parseInt.fold(succ = _.point[IO], fail = _ ⇒ throwIO(new RuntimeException("Invalid port '%s'".format(s))))
    } yield p

  def createServer(port: Int): IO[Server] = IO {
    val servlets = new ServletContextHandler
    servlets.setContextPath("/")
    servlets.addServlet(new ServletHolder(IssueLinkingServlet), "/issue-linker/*")

    val handlers = new HandlerList
    handlers.setHandlers(Array(servlets))

    val s = new Server(port)
    s.setHandler(handlers)
    s
  }

  def start(s: Server): IO[Unit] = IO {
    s.start()
    s.join()
  }
}

object IssueLinkingServlet extends HttpServlet {
  override def doGet(req: HttpServletRequest , resp: HttpServletResponse): Unit = {
    resp.getWriter.println("link!")
    resp.getWriter.flush
  }

}
