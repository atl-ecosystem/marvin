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
        println("received message '%s'".format(msg))
        val i = is.getOrElseUpdate(msg.room, new ScalaInterpreter(config.hipchatToken, msg.room))
        println("got interpreter for room '%s'".format(msg.room))
        i.interpret(msg.message.trim.drop(1))
      }
    }
    
    WebHookServlet(config.scalaReplKey) { msg ⇒
      interpreters ! msg
      None
    }
  }
}

private[this] class ScalaInterpreter(key: String, room: Room) {
  println("creating scala interpreter for room '%s'".format(room))
  import scala.tools.nsc.interpreter.{IMain}
  import scala.tools.nsc.interpreter.Results._

  def interpret(code: String): Unit = actor ! code

  implicit val SingleThreadedStrategy = Strategy.Executor(Executors.newSingleThreadExecutor)

  val out = new ByteArrayOutputStream
  private[this] lazy val si = {
    println("building IMain instance")
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

  private[this] def interpret_[A](code: String)(f: (Result, String) => A) =
    try {
      val r = si.interpret(code)
      f(r, out.toString)
    } finally {
      out.flush
      out.reset
    }
  private[this] val actor = Actor[String] { code ⇒
    Hipchat.sendMessage(key, Left(room.id), interpret_(code)((r, out) ⇒ r match {
      case Success => println("successfully interpreted"); out.replaceAll("(?m:^res[0-9]+: )", "") // + "\n" + iout.toString.replaceAll("(?m:^res[0-9]+: )", "")
      case Error => println("error interpreting"); out.replaceAll("^<console>:[0-9]+: ", "")
      case Incomplete => println("incomplete expression"); "error: unexpected EOF found, incomplete expression"
    }))
  }
}

