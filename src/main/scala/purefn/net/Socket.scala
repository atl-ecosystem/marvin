package purefn.net

import scalaz._
import effect._
import iteratee._
import Scalaz._

import java.io.{InputStream, InputStreamReader, Reader}
import java.net.{Socket => JSocket, SocketAddress, InetSocketAddress}
import javax.net.ssl.{SSLSocket => JSslSocket}

sealed trait Socket {
  private[net] val s: JSocket
  
  def close: IO[Unit] = IO(s.close)
  def connect(addr: SocketAddress):  IO[Unit] = IO(s.connect(addr))
  
  private def reader: IO[Reader] = IO(new InputStreamReader(s.getInputStream()))
  def inputStream: IO[InputStream] = IO(s.getInputStream())
}

trait Sockets {
  def socket: IO[Socket] = IO(new Socket {
    import javax.net.SocketFactory
    val s = SocketFactory.getDefault.createSocket()
  })
  
  def sslSocket: IO[Socket] = IO(new Socket {
    import javax.net.ssl.SSLSocketFactory
    val s = SSLSocketFactory.getDefault.createSocket()
  })
  
  import Net._
  def connectTo(hostname: HostName, p: PortID): IO[Socket] = p(connectToPort(hostname, _))

  private def connectToPort(hostname: HostName, port: Int) = socket.bracketOnError(_.close) { s =>
    for {
      address <- getAddressByName(hostname)
      _ <- s.connect(new InetSocketAddress(address, port))
    } yield s
  }  
}

object Socket extends Sockets