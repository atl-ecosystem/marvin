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

  private[this] val stdOut = System.out
  private[this] val stdErr = System.err
  private[this] val conOut = new ByteArrayOutputStream
  private[this] val conOutStream = new PrintStream(conOut)

  implicit val SingleThreadedStrategy = Strategy.Executor(Executors.newSingleThreadExecutor)

  private[this] lazy val si = {
    println("building IMain instance")
    val settings = new scala.tools.nsc.Settings(null)
    settings.usejavacp.value = true
    settings.deprecation.value = true
    settings.YdepMethTpes.value = true
    val si = new IMain(settings) { 
      override def parentClassLoader = Thread.currentThread.getContextClassLoader 
    }
    si.quietImport("scalaz._")
    si.quietImport("Scalaz._")
    si
  }
  private[this] def interpret_(code: String) = 
    try {
      System setOut conOutStream
      System setErr conOutStream
      println("interpreting '%s'".format(code))
      si.interpret(code)
    } finally {
      System setOut stdOut
      System setErr stdErr
      conOut.flush
      conOut.reset
    }

  private[this] val actor = Actor[String] { code ⇒
    Hipchat.sendMessage(key, Left(room.id), interpret_(code) match {
      case Success => println("successfully interpreted"); conOut.toString.replaceAll("(?m:^res[0-9]+: )", "") // + "\n" + iout.toString.replaceAll("(?m:^res[0-9]+: )", "")
      case Error => println("error interpreting"); conOut.toString.replaceAll("^<console>:[0-9]+: ", "")
      case Incomplete => println("incomplete expression"); "error: unexpected EOF found, incomplete expression"
    })
  }
}

