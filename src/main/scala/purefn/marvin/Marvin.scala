package purefn.marvin

import scalaz._
import concurrent._
import effect._
import std.option._
import std.set._
import std.string._
import syntax.equal._
import syntax.monad._
import syntax.plus._
import syntax.traverse._

import org.jivesoftware.smack._
import packet.{Message => XMessage, _}
import util.StringUtils.{parseName, parseResource}
import org.jivesoftware.smackx.muc._

sealed trait Marvin {
  def stop: IO[Unit]
}

sealed trait Command
case object Stop extends Command

object Marvin {
  sealed case class Config
    ( host: String
    , port: Int
    , username: String
    , password: String
    , rooms: Set[String]
    , conferenceServer: String
    , nick: String = "marvin"
    , ignore: Set[String] = Set()
    )

  def apply(config: Config)(processor: Processor): IO[Marvin] = {
    val m = new XmppMarvin(config, processor)
    m.connect.bracketOnError(_ => m.stop)(_ => m.login >> m.listen) >| m
  }
}

private[this] class XmppMarvin(config: Marvin.Config, processor: Processor) extends Marvin {
  val cc = new ConnectionConfiguration(config.host, config.port)
  val c = new XMPPConnection(cc)

  def connect: IO[Unit] = IO(c.connect)

  def login: IO[Unit] = IO(c.login(config.username, config.password, "marvin"))

  def listen: IO[Unit] = 
    joinRooms >> listenChat
  
  def joinRooms: IO[Unit] = 
    config.rooms.traverse_(r => joinRoom(r).except(e => IO.putStrLn("Could not join room `%s`: %s".format(r, e.getMessage))))

  def joinRoom(room: String): IO[Unit] = IO {
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
        msg.setBody(m.trim)
        muc.sendMessage(msg)
      }
      val ignore = config.ignore ++ config.nick
      def processPacket(p: Packet) = p match {
        case m: XMessage if !ignore(parseResource(m.getFrom)) => 
          processor.run(Right(mucMessage(m))) map (_ map reply)
          ()
        case _ => ()
      }
    })
  }

  def listenChat: IO[Unit] = IO {
    c.getChatManager.addChatListener(new ChatManagerListener {
      def chatCreated(chat: Chat, createdLocally: Boolean) =
        chat.addMessageListener(new MessageListener {
          def chatMessage(m: XMessage) =
            Message( from = parseName(m.getFrom)
                   , room = None
                   , body = m.getBody
                   )
          def reply(m: String) = chat.sendMessage(m.trim)
          def processMessage(chat: Chat, m: XMessage) = {
            processor.run(Right(chatMessage(m))) map (_ map reply)
            ()
          }
        })
    })
  }

  def stop = IO {
    processor.run(Left(Stop))
    c.disconnect()
  }
}
