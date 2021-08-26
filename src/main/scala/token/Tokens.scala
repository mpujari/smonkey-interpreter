// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package token

object Tokens {
  val ILLEGAL = "ILLEGAL"
  val EOF = "EOF"

  // identifiers and literals
  val IDENT = "IDENT"
  val INT = "INT"

  // operators
  val ASSIGN = "="
  val PLUS = "+"
  val SUB = "-"
  val DIV = "/"
  val MOD = "%"

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

  val keywords: Map[String, String] = Map(
    "fn" -> FUNCTION,
    "let" -> LET
  ).withDefaultValue(IDENT)

}
