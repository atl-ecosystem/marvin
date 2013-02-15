package purefn.marvin

sealed case class Message
  ( from: String
  , room: Option[Room]
  , body: String
  )

