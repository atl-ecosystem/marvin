package purefn.marvin

import scalaz._
import std.option._
import std.string._
import syntax.equal._
import syntax.plus._

import org.jivesoftware.smack._
import packet.{Message => XMessage, _}
import util.StringUtils.{parseName, parseResource}
import org.jivesoftware.smackx.muc._

sealed case class Config
  ( host: String
  , port: Int
  , username: String
  , password: String
  , jiraBase: String
  , rooms: Set[String]
  , conferenceServer: String
  , nick: String
  )

sealed trait Marvin {
  def stop: Unit
}

sealed trait Command
case object Stop extends Command

object Marvin {
  def apply(config: Config): Marvin = new Marvin {
    val process = ScalaRepl(config) <+> IssueLinker(config) 

    val cc = new ConnectionConfiguration(config.host, config.port)
    val c = new XMPPConnection(cc)
    c.connect()
    c.login(config.username, config.password, "marvin")

    config.rooms.foreach(joinRoom)

    c.getChatManager.addChatListener(new ChatManagerListener {
      def chatCreated(chat: Chat, createdLocally: Boolean) =
        chat.addMessageListener(new MessageListener {
          def chatMessage(m: XMessage) =
            Message( from = parseName(m.getFrom)
                   , room = None
                   , body = m.getBody
                   )
          def reply(m: String) = chat.sendMessage(m)
          def processMessage(chat: Chat, m: XMessage) = {
            process.run(Right(chatMessage(m))) map (_ map reply)
            ()
          }
        })
    })

    private[this] def joinRoom(room: String) = {
      val muc = new MultiUserChat(c, "%s@%s".format(room, config.conferenceServer))
      val history = new DiscussionHistory
      history.setMaxStanzas(0)
      muc.join(config.nick, null, history, SmackConfiguration.getPacketReplyTimeout)
      muc.addMessageListener(new PacketListener {
        def mucMessage(m: XMessage) = 
          Message( from = parseResource(m.getFrom)
                 , room = Some(parseName(m.getFrom))
                 , body = m.getBody
                 )
        def reply(m: String) = {
          val msg = muc.createMessage
          msg.setBody(m)
          muc.sendMessage(msg)
        }
        def processPacket(p: Packet) = p match {
          case m: XMessage if parseResource(m.getFrom) /== config.nick => 
            process.run(Right(mucMessage(m))) map (_ map reply)
            ()
          case _ => ()
        }
      })
    }

    def stop = {
      process.run(Left(Stop))
      c.disconnect()
    }
  }
}
