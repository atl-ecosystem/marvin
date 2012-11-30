package com.atlassian.ecosystem.marvin

import scalaz.concurrent._

import javax.servlet.http._

import java.io._
import java.util.concurrent._

object ScalaReplServlet {
  def apply(config: Config): HttpServlet = {
    implicit def SingleThreadedStrategy = Strategy.Executor(Executors.newSingleThreadExecutor)
    val interpreters = {
      var is = scala.collection.mutable.Map[Room, ScalaInterpreter]()
      Actor[WebHookMessage] { msg ⇒
        val i = is.getOrElseUpdate(msg.room, new ScalaInterpreter(config.hipchatToken, msg.room))
        i.interpret(msg.message.trim.drop(1))
      }
    }
    
    WebHookServlet(config.scalaReplKey) { msg ⇒
      interpreters ! msg
      None
    }
  }
}

private[this] class ScalaInterpreter(token: String, room: Room) {
  import scala.tools.nsc.interpreter.{IMain}
  import scala.tools.nsc.interpreter.Results._

  def interpret(code: String): Unit = actor ! code

  implicit val SingleThreadedStrategy = Strategy.Executor(Executors.newSingleThreadExecutor)

  val out = new ByteArrayOutputStream
  private[this] lazy val si = {
    val settings = new scala.tools.nsc.Settings(null)
    settings.usejavacp.value = true
    settings.deprecation.value = true
    settings.YdepMethTpes.value = true
    val si = new IMain(settings, new scala.tools.nsc.NewLinePrintWriter(new OutputStreamWriter(out), true)) { 
      override def parentClassLoader = Thread.currentThread.getContextClassLoader 
    }
    si.quietImport("scalaz._")
    si.quietImport("Scalaz._")
    si
  }

  private[this] def interpret_[A](code: String)(f: Result => A) =
    try {
      f(si.interpret(code))
    } finally {
      out.flush
      out.reset
    }
  private[this] def parseResult(r: Result) = r match {
    case Success => out.toString.replaceAll("(?m:^res[0-9]+: )", "") // + "\n" + iout.toString.replaceAll("(?m:^res[0-9]+: )", "")
    case Error => out.toString.replaceAll("^<console>:[0-9]+: ", "")
    case Incomplete => "error: unexpected EOF found, incomplete expression"
  }
  private[this] def message(r: Result) =
    Message( roomId = room.id
           , from = "marvin"
           , message = parseResult(r)
           )
  private[this] val actor = Actor[String] { code ⇒
    Hipchat.sendMessage(token, interpret_(code)(message))
  }
}

