package com.atlassian.ecosystem.marvin

import scalaz._
import syntax.validation._

import com.ephox.argonaut._
import Argonaut._

import org.apache.commons.fileupload._
import org.apache.commons.fileupload.disk._
import org.apache.commons.fileupload.servlet._

import javax.servlet.http._

object WebHookServlet {
  def apply(privateKey: String)(f: WebHookMessage ⇒ Option[Message]): HttpServlet = new HttpServlet {
    override def doPost(req: HttpServletRequest , resp: HttpServletResponse): Unit =
      parse(req) match {
        case Failure(InvalidKey) ⇒ resp.sendError(401)
        case Failure(ParseError(err)) ⇒ resp.sendError(400, err); println("Failed during parsing: " + err)
        case Success(in) ⇒ f(in) match {
          case Some(msg) ⇒ resp.setStatus(200)
                           sendMessage(resp, msg)
          case _         ⇒ resp.setStatus(202)
        }
      }

    def sendMessage(resp: HttpServletResponse, msg: Message): Unit = {
      resp.setContentType("application/json")
      resp.getWriter.write(implicitly[EncodeJson[Message]].apply(msg).toString)
      resp.getWriter.flush
    }

    private def multipartParser = new ServletFileUpload(new DiskFileItemFactory())
    private def parts(req: HttpServletRequest) = {
      import scala.collection.JavaConversions
      val fis = JavaConversions.asScalaBuffer(multipartParser.parseRequest(req).asInstanceOf[java.util.List[FileItem]]).toList 
      fis.flatMap(fi ⇒ if (fi.isFormField) List(fi.getFieldName → fi.getString) else Nil).toMap
    }
    private def parsePayload(payload: String) = {
      val res = payload.parseIgnoreErrorType( _.decode[WebHookMessage].map(_.success)
                                            , s ⇒ DecodeResult(s.fail)
                                            )
      res.toValidation.flatMap(identity).fail.map(ParseError(_)).validation
    }
    private def parse(req: HttpServletRequest): Validation[HipchatError, WebHookMessage] = {
      val ps = parts(req)
      ps.get("private_key") match {
        case Some(k) if k == privateKey ⇒
          ps.get("payload").map(parsePayload).getOrElse(ParseError("No payload").fail)
        case _ ⇒ InvalidKey.fail
      }
    }
  }
}
