package com.atlassian.ecosystem.marvin

import javax.servlet.http._

object LinkIssueServlet {
  def apply(config: Config): HttpServlet = WebHookServlet(config.linkIssueKey) { msg ⇒
    def links = (for (id <- LINK_PATTERN findAllIn msg.message) yield link(id)).toList.distinct
    def link(issueKey: String) = config.jiraBase + "browse/" + issueKey

    Some(Message( roomId = msg.room.id
                , from = "marvin"
                , message = links.mkString("\n")
                ))
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

