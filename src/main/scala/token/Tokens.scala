// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package token

object Tokens {

  type TokenType = String

  val ILLEGAL = "ILLEGAL"
  val EOF = "EOF"

  // identifiers and literals
  val IDENT = "IDENT"
  val INT = "INT"
  val FLOAT = "FLOAT"

  // operators
  val ASSIGN = "="
  val PLUS = "+"
  val MINUS = "-"
  val BANG = "!"
  val ASTERISK = "*"
  val SLASH = "/"
  val MOD = "%"

  val EQ = "=="
  val NOT_EQ = "!="
  val LT = "<"
  val GT = ">"
  val LT_EQ = "<="
  val GT_EQ = ">="

  // delimiters
  val COMMA = ","
  val SEMICOLON = ";"

  val LPAREN = "("
  val RPAREN = ")"
  val LBRACE = "{"
  val RBRACE = "}"

  // Keywords
  val FUNCTION = "FUNCTION"
  val LET = "LET"
  val IF = "if"
  val ELSE = "else"
  val TRUE = "true"
  val FALSE = "false"
  val RETURN = "return"

  val keywords: Map[String, String] = Map(
    "fn" -> FUNCTION,
    "let" -> LET,
    "if" -> IF,
    "else" -> ELSE,
    "true" -> TRUE,
    "false" -> FALSE,
    "return" -> RETURN
  ).withDefaultValue(IDENT)

}
