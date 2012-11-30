package com.atlassian.ecosystem.marvin

import javax.servlet.http._

import java.io._

object ScalaServlet {
  def apply(config: Config): HttpServlet = WebHookServlet(config.scalaReplKey) { msg ⇒
    Some(Message( roomId = msg.room.id
                , from = "marvin"
                , message = msg.message
                ))
  }
  
  import scala.tools.nsc.interpreter.{IMain}

  val scalaInt = scala.collection.mutable.Map[String, IMain]()
  def scalaInterpreter(channel: String)(f: (IMain, ByteArrayOutputStream) => Unit) = this.synchronized {
    val si = scalaInt.getOrElseUpdate(channel, {
      val settings = new scala.tools.nsc.Settings(null)
      settings.usejavacp.value = true
      settings.deprecation.value = true
      settings.YdepMethTpes.value = true
      val si = new IMain(settings) { override def
        parentClassLoader = Thread.currentThread.getContextClassLoader 
      }
      si.quietImport("scalaz._")
      si.quietImport("Scalaz._")
      si.quietImport("org.scalacheck.Prop._")
      si
    })
    captureOutput{f(si, conOut)}
  }

  val stdOut = System.out
  val stdErr = System.err
  val conOut = new ByteArrayOutputStream
  val conOutStream = new PrintStream(conOut)

  def captureOutput(block: => Unit) = 
    try {
      System setOut conOutStream
      System setErr conOutStream
      block
    } finally {
      System setOut stdOut
      System setErr stdErr
      conOut.flush
      conOut.reset
    }
}
