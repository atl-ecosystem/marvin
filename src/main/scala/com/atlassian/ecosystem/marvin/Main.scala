package com.atlassian.ecosystem.marvin

import scalaz.{Source ⇒ _, _}
import effect._
import IO._
import syntax.monad._
import syntax.validation._
import syntax.std.option._
import syntax.std.string._

import com.ephox.argonaut._
import Argonaut._

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.handler.{ContextHandler, ResourceHandler, HandlerList}
import org.eclipse.jetty.servlet.{ServletHolder, ServletContextHandler}

import scala.io._

import javax.servlet.http._

case class Config(port: Int, hipchatToken: String, issueLinkToken: String)

object Main {
  def main(args: Array[String]): Unit = mainIO(ImmutableArray.fromArray(args)) unsafePerformIO

  def mainIO(args: ImmutableArray[String]): IO[Unit] = 
    getConfig >>= createServer >>= start
  
  type ConfigParseResult[A] = ValidationNEL[String, A]
  implicit val ApplyIOConfigParseResult = Apply[IO].compose[ConfigParseResult]
  def getConfig: IO[Config] =
    ( getPort         |@|
      getHipchatToken |@|
      getIssueLinkToken
    )(Config.apply) >>= (_.fold(succ = _.point[IO], fail = errs ⇒ throwIO(new RuntimeException(errs.list.mkString))))

  def getEnv(name: String): IO[Option[String]] = IO(Option(System.getenv(name)))

  def getPort: IO[ConfigParseResult[Int]] =
    getEnv("PORT") map (s ⇒
      s.getOrElse("8080").parseInt.fold(succ = _.successNel, fail = _ ⇒ "Invalid port '%s'".format(s).failNel))

  def getHipchatToken: IO[ConfigParseResult[String]] =
    getEnv("HIPCHAT_TOKEN") map (_.fold(some = _.successNel, none = "Missing env var 'HIPCHAT_TOKEN'".failNel))

  def getIssueLinkToken: IO[ConfigParseResult[String]] =
    getEnv("ISSUE_LINK_TOKEN") map (_.fold(some = _.successNel, none = "Missing env var 'ISSUE_LINK_TOKEN'".failNel))

  def createServer(config: Config): IO[Server] = IO {
    val servlets = new ServletContextHandler
    servlets.setContextPath("/")
    servlets.addServlet(new ServletHolder(new IssueLinkingServlet(config)), "/issue-linker")

    val handlers = new HandlerList
    handlers.setHandlers(Array(servlets))

    val s = new Server(config.port)
    s.setHandler(handlers)
    s
  }

  def start(s: Server): IO[Unit] = IO {
    s.start()
    s.join()
  }
}


case class Room(id: Int, name: String)
object Room {
  implicit lazy val decodeJsonRoom: DecodeJson[Room] =
    DecodeJson(j => j.array match {
      case Some(id::n::Nil) =>
        for {
          id_ <- implicitly[DecodeJson[Int]].apply(id)
          n_ <- implicitly[DecodeJson[String]].apply(n)
        } yield Room(id_, n_)
      case _ => DecodeResult.decodeError(j, "Room")
    })
}
case class Sender(id: Int, name: String, mention: String)
object Sender {
  implicit lazy val decodeJsonSender: DecodeJson[Sender] =
    DecodeJson(j => j.array match {
      case Some(id::n::m::Nil) =>
        for {
          id_ <- implicitly[DecodeJson[Int]].apply(id)
          n_ <- implicitly[DecodeJson[String]].apply(n)
          m_ <- implicitly[DecodeJson[String]].apply(m)
        } yield Sender(id_, n_, m_)
      case _ => DecodeResult.decodeError(j, "Sender")
    })
}
case class WebHookMessage(regex: String, matches: List[String], message: String, room: Room, sender: Sender)
object WebHookMessage {
  implicit lazy val decodeJsonWebHookMessage: DecodeJson[WebHookMessage] =
    DecodeJson(j => j.array match {
      case Some(r::ms::msg::rm::s::Nil) =>
        for {
          r_  <- implicitly[DecodeJson[String]].apply(r)
          ms_ <- implicitly[DecodeJson[List[String]]].apply(ms)
          msg_ <- implicitly[DecodeJson[String]].apply(msg)
          rm_ <- implicitly[DecodeJson[Room]].apply(rm)
          s_ <- implicitly[DecodeJson[Sender]].apply(s)
        } yield WebHookMessage(r_, ms_, msg_, rm_, s_)
      case _ => DecodeResult.decodeError(j, "WebHookMessage")
    })
}
sealed trait MessageFormat
object MessageFormat {
  case object Html extends MessageFormat
  case object Text extends MessageFormat

  implicit lazy val encodeJsonMessageFormat: EncodeJson[MessageFormat] =
    EncodeJson(f => implicitly[EncodeJson[String]].apply(f.toString.toLowerCase), "MessageFormat")
}
sealed trait MessageColor
object MessageColor {
  case object Yellow extends MessageColor
  case object Red extends MessageColor
  case object Green extends MessageColor
  case object Purple extends MessageColor
  case object Gray extends MessageColor
  case object Random extends MessageColor

  implicit lazy val encodeJsonMessageColor: EncodeJson[MessageColor] =
    EncodeJson(c => implicitly[EncodeJson[String]].apply(c.toString.toLowerCase), "MessageColor")
}
case class Message
  ( roomId: Int
  , from: String
  , message: String
  , color: MessageColor = MessageColor.Yellow
  , format: MessageFormat = MessageFormat.Text
  )
object Message {
  private def messageToJson(msg: Message) = msg match {
    case Message(r, f, m, c, fmt) => 
      jArray(List( implicitly[EncodeJson[Int]].apply(r)
                 , implicitly[EncodeJson[String]].apply(f)
                 , implicitly[EncodeJson[String]].apply(m)
                 , implicitly[EncodeJson[MessageColor]].apply(c)
                 , implicitly[EncodeJson[MessageFormat]].apply(fmt)
                 ))
  }
  implicit lazy val EncodePerson: EncodeJson[Message] = EncodeJson(messageToJson, "Message")
}

sealed trait HipchatError
case object InvalidKey extends HipchatError
sealed case class ParseError(err: String) extends HipchatError

class IssueLinkingServlet(config: Config) extends HttpServlet {
  def parse(req: HttpServletRequest): Validation[HipchatError, WebHookMessage] =
    Option(req.getParameter("private_key")) match {
      case Some(k) if k == config.issueLinkToken ⇒
        val in = req.getParameter("payload")
        val res = in.parseIgnoreErrorType( _.decode[WebHookMessage].map(_.success)
                                         , s ⇒ DecodeResult(s.fail)
                                        )
        res.toValidation.flatMap(identity).swap.map(ParseError(_)).swap
      case _ ⇒ InvalidKey.fail
    }
  override def doPost(req: HttpServletRequest , resp: HttpServletResponse): Unit = {
    import scala.collection.JavaConversions._
    for { h <- req.getHeaderNames } println("%s: %s".format(h, req.getHeaders(h).mkString(",")))
    for { p <- req.getParameterMap } println("%s=%s".format(p._1, p._2))
    println(Source.fromInputStream(req.getInputStream).mkString(""))

    parse(req) match {
      case Failure(InvalidKey) ⇒ resp.sendError(401)
      case Failure(ParseError(err)) ⇒ resp.sendError(400, err); println(err)
      case Success(in) ⇒
        resp.setContentType("application/json")
        val out = Message( roomId = in.room.id
                         , from = "marvin"
                         , message = in.message
                         )
        resp.getWriter.write(implicitly[EncodeJson[Message]].apply(out).toString)
        resp.getWriter.flush
    }
  }

  // the constructed regex says to find issue keys:
  // * with whitespace, nothing, or an open parenthesis before them. (open paren is here to allow the case where the key is in parenthesis, though the regex doesn't check that the key also ends with one -- unnecessary complication for an uncommon case, i think) 
  // * containing at least two uppercase letters followed by a dash and then at least one digit
  // * followed by a word boundary (first non-digit or non-character, such as a space or period)
  val PRE_LINK_PATTERN_STRING = "(?<!" + // start non-capturing group, zero-width negative lookbehind
  		                          "[^\\s(]" + // character class containing all non-whitespace or non-openparen characters
  		                        ")" // end of group
  val POST_LINK_PATTERN_STRING = "\\b" // a word boundary
  val LINK_PATTERN_STRING = PRE_LINK_PATTERN_STRING + 
                            "(" + // start of capturing group
                              "\\p{Lu}{2,}" +  // an uppercase letter, at least two times
                              "-" + // a dash
                              "\\p{Digit}+" + // a digit, one or more times
                            ")" + // end of capturing group
                            POST_LINK_PATTERN_STRING
  val LINK_PATTERN = LINK_PATTERN_STRING.r
}

