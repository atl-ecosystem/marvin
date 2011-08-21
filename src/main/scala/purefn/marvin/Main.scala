package purefn.marvin

import scalaz._, effect._, iteratee._
import Marvin._

import java.io._

object Main {
  def main(args: Array[String]) { mainIO(args) except printStackTrace unsafePerformIO }
  
  def mainIO(args: Array[String]): IO[Unit] = 
      connectTo("irc.freenode.com", portNumber(6667)).bracket(_.close)(socket =>
        for { reader <- socket.inputStream flatMap bufferReader
              _ <- (putStrTo[IOException, String](System.out) %=
                mapErrorOr((_:IoExceptionOr[String]).fold(_.left, s => (s + "\n").right)) >>== 
                  enumLines(reader)) runT(IO.throwIO(_))
        } yield ()
     )
//    val io = (for { a <- inIoExceptionOr(throwIO[Int](new IOException("bad things")))
//          b <- inIoExceptionOr(throwIO[Int](new IOException("more bad things")))
//    } yield a.fold(ioe => ioException[Int](ioe), x => b.fold(ioe => ioException[Int](ioe), y => ioExceptionOr(x + y))))
//    io flatMap ( a => a.fold(e => putStr(e.getMessage), x => putStr(x.shows)) flatMap (_ => throwIO[Unit](new IOException("even worse things"))))
//    
  def inIoExceptionOr[A](io: IO[A]) = io map (ioExceptionOr(_)) catchSome (
    _ match { 
      case e: IOException => Some(e)
      case _ => None
    }, ioException[A](_: IOException).point[IO])
      
  def printStackTrace(e: Throwable) = IO(e.printStackTrace)
  
  def bufferReader(is: InputStream): IO[BufferedReader] = IO(new BufferedReader(new InputStreamReader(is)))
  
  def enumLines[X, A](r: BufferedReader): EnumeratorT[X, IoExceptionOr[String], IO, A] = {
    def loop: EnumeratorT[X, IoExceptionOr[String], IO, A] = { s =>
      s.mapContOr(
        k => {
          val i = IoExceptionOr(r.readLine)
          if (i exists (_ != null)) k(elInput(i)) >>== loop
          else s.pointI
        }
      , s.pointI
      )
    }
    loop
  }
}

