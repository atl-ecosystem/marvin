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

import org.apache.commons.fileupload._
import org.apache.commons.fileupload.disk._
import org.apache.commons.fileupload.servlet._

import scala.io._

import javax.servlet.http._

case class Config(port: Int, hipchatToken: String, issueLinkToken: String)

object Main {
  def main(args: Array[String]): Unit = mainIO(ImmutableArray.fromArray(args)) unsafePerformIO

  def mainIO(args: ImmutableArray[String]): IO[Unit] = 
    getConfig >>= createServer >>= start
  
  type ConfigParseResult[A] = ValidationNEL[String, A]
  def getConfig: IO[Config] =
    Apply[IO].compose[ConfigParseResult].map3( getPort
                                             , getHipchatToken
                                             , getIssueLinkToken
                                             )(Config.apply) >>= 
      (_.fold(success = _.point[IO], failure = errs ⇒ throwIO(new RuntimeException(errs.list.mkString))))

  def getEnv(name: String): IO[Option[String]] = IO(Option(System.getenv(name)))

  def getPort: IO[ConfigParseResult[Int]] =
    getEnv("PORT") map (s ⇒
      s.getOrElse("8080").parseInt.fold(success = _.successNel, failure = _ ⇒ "Invalid port '%s'".format(s).failNel))

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


object JsonDecoders {
  implicit lazy val IntDecodeJson: DecodeJson[Int] =
    DecodeJson.decodej(j => j.number match {
      case Some(n) => Some(n.toInt)
      case _ => j.string flatMap (s => try { Some(s.toInt) } catch { case _ => None })
    }, "Int")

  def decodeJsonObject[A](j: Json)(decode: JsonObject ⇒ DecodeResult[A]): DecodeResult[A] =
    j.obj.map(decode).getOrElse(DecodeResult.decodeError(j, "not a json object"))

  def reqObjField[A](jobj: JsonObject, oname: String, name: JsonField)(implicit A: DecodeJson[A]): DecodeResult[A] = {
    def decode(f: Json): DecodeResult[A] = {
      val r = A.apply(f)
      r.toEither match {
        case Left(msg) => DecodeResult.decodeError(r.json.get, "Could not parse '%s.%s' value '%s': %s".format(oname, name, f, msg))
        case Right(a)  ⇒ DecodeResult(a)
      }
    }
    jobj(name).map(decode).getOrElse(DecodeResult.decodeError(jObject(jobj), "missing '%s.%s' field".format(oname, name)))
  }
}

object JsonEncoders {
  def jsonObject(fs: (JsonField, Json)*): Json =
    jObject(fs.foldLeft(JsonObject.empty)((j, f) ⇒ j + (f._1, f._2)))
}

case class Room(id: Int, name: String)
object Room {
  import JsonDecoders._
  implicit lazy val decodeJsonRoom: DecodeJson[Room] =
    DecodeJson(decodeJsonObject(_)(jobj ⇒
      ( reqObjField[Int](jobj, "Room", "id")      |@|
        reqObjField[String](jobj, "Room", "pretty_name")
      )(Room.apply)
    ))
}
case class Sender(id: Int, name: String, mention: String)
object Sender {
  import JsonDecoders._
  implicit lazy val decodeJsonSender: DecodeJson[Sender] =
    DecodeJson(decodeJsonObject(_)(jobj ⇒
      ( reqObjField[Int](jobj, "Sender", "id")         |@|
        reqObjField[String](jobj, "Sender", "name")    |@|
        reqObjField[String](jobj, "Sender", "mention_name")
      )(Sender.apply)
    ))
}

case class WebHookMessage(regex: String, matches: List[String], message: String, room: Room, sender: Sender)
object WebHookMessage {
  import JsonDecoders._
  implicit lazy val decodeJsonWebHookMessage: DecodeJson[WebHookMessage] =
    DecodeJson(decodeJsonObject(_)(jobj ⇒
      ( reqObjField[String](jobj, "WebHookMessage", "regex")            |@|
        reqObjField[List[String]](jobj, "WebHookMessage", "matches")    |@|
        reqObjField[String](jobj, "WebHookMessage", "message")          |@|
        reqObjField[Room](jobj, "WebHookMessage", "room")               |@|
        reqObjField[Sender](jobj, "WebHookMessage", "sender")
      )(WebHookMessage.apply)
    ))
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
  , notifyR: Boolean = false
  )
object Message {
  import JsonEncoders._
  private def messageToJson(msg: Message) =
    jsonObject( "room_id" → jNumber(JsonNumber(msg.roomId))
              , "from"    → implicitly[EncodeJson[String]].apply(msg.from)
              , "message" → implicitly[EncodeJson[String]].apply(msg.message)
              , "color"   → implicitly[EncodeJson[MessageColor]].apply(msg.color)
              , "format"  → implicitly[EncodeJson[MessageFormat]].apply(msg.format)
              , "notify"  → jNumber(JsonNumber(if (msg.notifyR) 1 else 0))
              )
  implicit lazy val EncodePerson: EncodeJson[Message] = EncodeJson(messageToJson, "Message")
}

sealed trait HipchatError
case object InvalidKey extends HipchatError
sealed case class ParseError(err: String) extends HipchatError

class IssueLinkingServlet(config: Config) extends HttpServlet {
  def multipartParser = new ServletFileUpload(new DiskFileItemFactory())
  def parts(req: HttpServletRequest) = {
    import scala.collection.JavaConversions
    val fis = JavaConversions.asScalaBuffer(multipartParser.parseRequest(req).asInstanceOf[java.util.List[FileItem]]).toList 
    fis.flatMap(fi ⇒ if (fi.isFormField) List(fi.getFieldName → fi.getString) else Nil).toMap
  }
  def parsePayload(payload: String) = {
    val res = payload.parseIgnoreErrorType( _.decode[WebHookMessage].map(_.success)
                                          , s ⇒ DecodeResult(s.fail)
                                          )
    res.toValidation.flatMap(identity).fail.map(ParseError(_)).validation
  }
  def parse(req: HttpServletRequest): Validation[HipchatError, WebHookMessage] = {
    val ps = parts(req)
    ps.get("private_key") match {
      case Some(k) if k == config.issueLinkToken ⇒
        ps.get("payload").map(parsePayload).getOrElse(ParseError("No payload").fail)
      case _ ⇒ InvalidKey.fail
    }
  }
  override def doPost(req: HttpServletRequest , resp: HttpServletResponse): Unit = {
    parse(req) match {
      case Failure(InvalidKey) ⇒ resp.sendError(401)
      case Failure(ParseError(err)) ⇒ resp.sendError(400, err); println("Failed during parsing: " + err)
      case Success(in) ⇒
        resp.setContentType("application/json")
        val out = Message( roomId = in.room.id
                         , from = "marvin"
                         , message = in.message
                         )
//         val outstr = """{"room_id":121054,"from":"marvin","message":"dfa AMKT-123","color":"yellow","format":"text","notify":0}"""
//         resp.getWriter.write(outstr)
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

