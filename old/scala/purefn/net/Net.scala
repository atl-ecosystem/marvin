package purefn.net

import scalaz._
import Scalaz._
import effect._

import java.net._

trait Nets extends Sockets {
  type HostName = String
  
  def portNumber(port: Int) = new PortID {
    def apply[A](portNumber: Int => A) = portNumber(port)
  }
    
  def getAddressByName(name: HostName): IO[InetAddress] = IO { InetAddress.getByName(name) }
}

object Net extends Nets

sealed trait PortID {
  def apply[A](portNumber: Int => A): A
}
