package com.atlassian.ecosystem.marvin

import com.ephox.argonaut._
import Argonaut._

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
