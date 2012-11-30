package com.atlassian.ecosystem.marvin

import scalaz._
import std.anyVal._
import syntax.monad._
import syntax.show._

import com.ephox.argonaut._
import Argonaut._

import javax.servlet.http._

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
sealed trait MessageFormat {
  import MessageFormat._
  override final val toString = this match {
    case Html ⇒ "html"
    case Text ⇒ "text"
  }
}
object MessageFormat {
  case object Html extends MessageFormat
  case object Text extends MessageFormat

  implicit lazy val encodeJsonMessageFormat: EncodeJson[MessageFormat] =
    EncodeJson(f => implicitly[EncodeJson[String]].apply(f.toString.toLowerCase), "MessageFormat")

  implicit lazy val MessageFormatShow: Show[MessageFormat] = Show.showA
}
sealed trait MessageColor {
  import MessageColor._
  override final val toString = this match {
    case Yellow ⇒ "yellow"
    case Red    ⇒ "red"
    case Green  ⇒ "green"
    case Purple ⇒ "purple"
    case Gray   ⇒ "gray"
    case Random ⇒ "random"
  }
}
object MessageColor {
  case object Yellow extends MessageColor
  case object Red extends MessageColor
  case object Green extends MessageColor
  case object Purple extends MessageColor
  case object Gray extends MessageColor
  case object Random extends MessageColor

  implicit lazy val encodeJsonMessageColor: EncodeJson[MessageColor] =
    EncodeJson(c => implicitly[EncodeJson[String]].apply(c.toString), "MessageColor")

  implicit lazy val MessageColorShow: Show[MessageColor] = Show.showA
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
    jsonObject( "room_id"         → jNumber(JsonNumber(msg.roomId))
              , "from"            → implicitly[EncodeJson[String]].apply(msg.from)
              , "message"         → implicitly[EncodeJson[String]].apply(msg.message)
              , "color"           → implicitly[EncodeJson[MessageColor]].apply(msg.color)
              , "message_format"  → implicitly[EncodeJson[MessageFormat]].apply(msg.format)
              , "notify"          → jNumber(JsonNumber(if (msg.notifyR) 1 else 0))
              )
  implicit lazy val EncodePerson: EncodeJson[Message] = EncodeJson(messageToJson, "Message")
}

sealed trait HipchatError
case object InvalidKey extends HipchatError
sealed case class ParseError(err: String) extends HipchatError

object Hipchat {
  import dispatch._
  val message = "https://api.hipchat.com/v1/rooms/message"
  def sendMessage(token: String, msg: Message) = {
    def formParams = Map( "room_id"     → msg.roomId.shows
                        , "from"        → msg.from
                        , "message"     → msg.message
                        , "color"       → msg.color.shows
                        , "message_format" → msg.format.shows
                        , "notify"      → (if (msg.notifyR) 1 else 0).shows
                        )
    Http(url(message) <<? Map("auth_token" → token) << formParams) foreach {res =>
      println(res.getStatusCode)
      println(res.getResponseBody)
    }
  }
}
