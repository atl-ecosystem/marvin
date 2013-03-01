package purefn

import scalaz._
import concurrent._

package object marvin {
  type Room = String
  type Nick = String

  type Processor = Kleisli[Option, Either[Command, Message], Promise[String]]

  def ??? = sys.error("Not implemented")
}

