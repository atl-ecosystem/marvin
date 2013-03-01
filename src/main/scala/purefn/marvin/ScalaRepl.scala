package purefn.marvin

import scalaz._
import concurrent._
import syntax.monad._

import scala.collection.mutable.ConcurrentMap

import java.io._
import java.util.concurrent._

object ScalaRepl {
  sealed case class Config(initCommands: List[String])

  def apply(config: Config): Processor = Kleisli {
    val interpreters: ConcurrentMap[Either[Room, Nick], Promise[ScalaInterpreter]] = {
      import scala.collection.JavaConversions._
      import java.util.concurrent.ConcurrentHashMap
      new ConcurrentHashMap[Either[Room, Nick], Promise[ScalaInterpreter]]()
    }

    _ match {
      case Left(Stop) =>
        interpreters.values.foreach(_.map(_.stop))
        None
      case Right(msg) =>
        val src = msg.room.map(Left(_)).getOrElse(Right(msg.from))
        def interpreter = interpreters.getOrElseUpdate(src, ScalaInterpreter(src.fold(_.toString, _.toString), config))
        val body = msg.body.trim

        if (body.startsWith(":t")) Some(interpreter >>= (_.typeOf(body.drop(2))))
        else if (body.startsWith(">")) Some(interpreter >>= (_.interpret(body.drop(1))))
        else None
    }
  }
}

private[this] class ScalaInterpreter(name: String, config: ScalaRepl.Config)(implicit s: Strategy) {
  import scala.tools.nsc.interpreter._
  import scala.tools.nsc.interpreter.Results._
  import scala.tools.nsc.util._

  def typeOf(expr: String): Promise[String] =
    Promise(si.typeOfExpression(expr) map (_.toString) getOrElse "Failed to determine type.")

  def interpret(expr: String): Promise[String] = {
    def eval = 
      try {
        si.interpret(expr) match {
          case Success => out.toString.replaceAll("(?m:^res[0-9]+: )", "")
          case Error => out.toString.replaceAll("^<console>:[0-9]+: ", "")
          case Incomplete => "error: unexpected EOF found, incomplete expression"
        }
      } finally {
        out.flush
        out.reset
      }
    Promise(eval)
  }

  def stop = si.close

  private[this] val out = new ByteArrayOutputStream
  private[this] val si = {
    val settings = new scala.tools.nsc.Settings(null)
    settings.usejavacp.value = true
    settings.deprecation.value = true
    settings.YdepMethTpes.value = true
    val si = new IMain(settings, new scala.tools.nsc.NewLinePrintWriter(new OutputStreamWriter(out), true))
    config.initCommands.foreach(si.quietRun)
    si
  }
}

private[this] object ScalaInterpreter {
  def apply(name: String, config: ScalaRepl.Config): Promise[ScalaInterpreter] = {
    implicit val SingleThreadedStrategy = Strategy.Executor(Executors.newSingleThreadExecutor(new ThreadFactory {
      def newThread(r: Runnable) = {
        val t = Executors.defaultThreadFactory.newThread(r)
        t.setName("interpreter-%s-thread".format(name))
        t.setDaemon(true)
        t
      }
    }))

    Promise(new ScalaInterpreter(name, config))
  }
}
