package purefn.marvin

// object IssueRaiser {
//   val usage = "raise <issue-type> <issue-summary> in <project-key> [reported by <user>] [assign to (<user>|me)] [schedule for <version>]"
//   def apply(config: Config): HttpServlet = WebHookServlet(config.raiseIssueKey) { msg ⇒
//     def parse = Parser.parse(msg.message.trim.drop(1))
//     def addUsage(v: Validation[String, RaiseIssue]) =
//       v.fold( success = _.toString
//             , failure = err ⇒ "Parsing failed: %s.  Usage: %s".format(err, usage)
//             )
//     Some(Message( roomId = msg.room.id
//                 , from = "marvin"
//                 , message = addUsage(parse)
//                 ))
//   }
// 
//   def raise(issue: RaiseIssue)(implicit messageFilter: Message => Boolean) = withSession { implicit jira =>
//     val project = jira.getProjectByKey(issue.project)
//     
//     def set(proto: Either[String, RemoteIssue])(f: RemoteIssue => Either[String, RemoteIssue]): Either[String, RemoteIssue] = 
//       proto.right.flatMap(f)
//     val setProject = (proto: Either[String, RemoteIssue]) => set(proto) { i => i.setProject(issue.project); Right(i) }
//     val setType = (proto: Either[String, RemoteIssue]) => set(proto) { i => i.setType(getTypeId(issue.issueType, project)); Right(i) }
//     val setSummary = (proto: Either[String, RemoteIssue]) => set(proto) { i => i.setSummary(issue.summary); Right(i) }
//     val setDescription = (proto: Either[String, RemoteIssue]) => set(proto) { i => i.setDescription(issue.timeInterval.map(messagesIn).getOrElse("")); Right(i) }
//     val setReporter = (proto: Either[String, RemoteIssue]) => set(proto) { i => issue.reporter.foreach(i.setReporter); Right(i) }
//     val setAssignee = (proto: Either[String, RemoteIssue]) => set(proto) { i => issue.assignee.foreach(i.setAssignee); Right(i) } 
//     val setFixVersion = (proto: Either[String, RemoteIssue]) => set(proto) { i =>
//       issue.version.map { vs =>
//         getVersion(vs, project).map { v => i.setFixVersions(Array(v)); Right(i) }.
//         getOrElse(Left("Unknown version '" + vs + "' for project '" + issue.project + "'"))
//       }.getOrElse(Right(i))
//     }
//     val send = (proto: Either[String, RemoteIssue]) => proto.right.map(jira.createIssue)
//     
//     val create = (setProject andThen setType andThen setSummary andThen setDescription andThen setReporter andThen
//         setAssignee andThen setFixVersion andThen send)
//     
//     create(Right(new RemoteIssue())) match {
//       case Left(err) => List(err)
//       case Right(createdIssue) => List("Created issue " + link(createdIssue.getKey()))
//     }
//   }
// }
// 
// import scala.util.parsing.combinator._
// import scala.util.parsing.combinator.syntactical._
// object Parser extends StandardTokenParsers {
//   lexical.delimiters ++= List(":", ",", "-")
//   lexical.reserved += ( "raise", "in", "by", "as", "on", "schedule", "for", "reported", "assign", "to")
// 
//   def parse(msg: String): Validation[String, RaiseIssue] = raise(new lexical.Scanner(msg)) match {
//     case Success(c, _) => c.success
//     case Failure(msg, _) => msg.fail
//     case Error(msg, _) => msg.fail
//   }
// 
//   def raise: Parser[RaiseIssue] =
//     typeAndSummary ~ project ~ opt(reportedBy) ~ opt(assignTo) ~ opt(scheduleFor) ^^ {
//       case typeAndSummary ~ projectKey ~ reporter ~ assignee ~ version => 
//         RaiseIssue(typeAndSummary._1, typeAndSummary._2, projectKey, reporter, assignee, version)
//     }
// 
//   def typeAndSummary: Parser[(String, String)] = 
//     "raise" ~ (ident | stringLit) ~ stringLit ^^ { case "raise" ~ t ~ s => (t, s) }
//   
//   def project: Parser[String] =
//     "in" ~> ident ^^ ( s => s )
//   
//   def assignTo: Parser[String] =
//     "assign" ~ "to" ~> ident ^^ (s => s)
//   
//   def scheduleFor: Parser[String] =
//     "schedule" ~ "for" ~> (ident | stringLit) ^^ (s => s)
//     
//   def reportedBy: Parser[String] =
//     "reported" ~ "by" ~> ident ^^ (s => s)
// }
// 
// case class RaiseIssue
//   ( issueType: String
//   , summary: String
//   , project: String 
//   , reporter: Option[String] 
//   , assignee: Option[String] 
//   , version: Option[String]
//   )
