package lexer

import token.Token
import token.Tokens._

trait Lexer {
  val input: String

  private val NIL: Char = '\u0000'

  private var position: Int = 0
  private var readPosition: Int = 0
  private var ch: Char = NIL

  def readChar(): Unit =
    if (readPosition >= input.length) {
      ch = NIL
    } else {
      ch = input.charAt(readPosition)
      position = readPosition
      readPosition = readPosition + 1
    }

  def nextToken(): Token = {
    skipWhiteSpace()
    val chStr = ch.toString
    val tok = ch match {
      case '=' => Token(ASSIGN, chStr)
      case ';' => Token(SEMICOLON, chStr)
      case '(' => Token(LPAREN, chStr)
      case ')' => Token(RPAREN, chStr)
      case ',' => Token(COMMA, chStr)
      case '+' => Token(PLUS, chStr)
      case '{' => Token(LBRACE, chStr)
      case '}' => Token(RBRACE, chStr)
      case NIL => Token(EOF, "")
      case _ =>
        if (isLetter(ch)) {
          val literal = readIdentifier()
          val `type` = keywords(literal)
          return Token(`type`, literal)
        } else if (isDigit(ch)) {
          return Token(INT, readNumber())
        } else {
          Token(ILLEGAL, chStr)
        }
    }

    readChar()
    tok
  }

  def isLetter(ch: Char): Boolean = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')

  def isDigit(ch: Char): Boolean = ch >= '0' && ch <= '9'

  def readIdentifier(): String = {
    val pos = position
    while (isLetter(ch)) {
      readChar()
    }
    input.substring(pos, position)
  }

  def readNumber(): String = {
    val pos = position
    while (isDigit(ch)) {
      readChar()
    }
    input.substring(pos, position)
  }

  def skipWhiteSpace(): Unit =
    while (ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r') {
      readChar()
    }
}

object Lexer {

  def apply(inputText: String): Lexer = new Lexer() {
    override val input: String = inputText
    readChar() // Init
  }

}
