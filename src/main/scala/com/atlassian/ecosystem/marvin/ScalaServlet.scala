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
        val msg_ = msg.message.trim
        if (msg_.startsWith(">type")) i.typed(msg_.drop(5))
        else i.interpret(msg_.drop(1))
      }
    }
    
    WebHookServlet(config.scalaReplKey) { msg ⇒
      interpreters ! msg
      None
    }
  }
}

sealed trait Expr { def expr: String }
sealed case class TypeExpr(expr: String) extends Expr
sealed case class InterpretExpr(expr: String) extends Expr
class ScalaInterpreter(token: String, room: Room) {
  import scala.tools.nsc.interpreter._
  import scala.tools.nsc.interpreter.Results._
  import scala.tools.nsc.util._

  def interpret(expr: String): Unit = actor ! InterpretExpr(expr)
  def typed(expr: String): Unit = actor ! TypeExpr(expr)

  implicit val SingleThreadedStrategy = Strategy.Executor(Executors.newSingleThreadExecutor)

  val out = new ByteArrayOutputStream
  lazy val si = {
    val settings = new scala.tools.nsc.Settings(null)
    settings.usejavacp.value = true
    settings.deprecation.value = true
    settings.YdepMethTpes.value = true
    val si = new IMain(settings, new scala.tools.nsc.NewLinePrintWriter(new OutputStreamWriter(out), true)) { 
      private var _classLoader: AbstractFileClassLoader = null
      override def resetClassLoader() = {
        _classLoader = mkClassLoader
      }
      override lazy val compilerClasspath = List(this.getClass.getClassLoader.getResource("amkt.jar"))
      private def mkClassLoader = {
        val parent = ScalaClassLoader.fromURLs(compilerClasspath, null)
        // copied from IMain and modified 
        new AbstractFileClassLoader(virtualDirectory, parent) {
          override protected def findAbstractFile(name: String) =
            super.findAbstractFile(name) match {
              case null if isInitializeComplete => generatedName(name) map (x => super.findAbstractFile(x)) orNull
              case file                         => file
            }
        }
      }
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
  private[this] def message(msg: String) =
    Message( roomId = room.id
           , from = "marvin"
           , message = msg
           )
  def run(expr: Expr) = expr match {
    case TypeExpr(e)      ⇒ message(si.typeOfExpression(e) map (_.toString) getOrElse "Failed to determine type.")
    case InterpretExpr(e) ⇒ message(interpret_(e)(parseResult))
  }
  private[this] val actor = Actor[Expr] { expr ⇒
    Hipchat.sendMessage(token, run(expr))
  }
}

