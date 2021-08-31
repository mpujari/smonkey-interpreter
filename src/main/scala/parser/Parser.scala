// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package parser

import ast.{Identifier, LetStatement, Program, Statement}
import lexer.Lexer
import token.Token
import token.Tokens._

import scala.collection.mutable.ListBuffer

trait Parser {

  val lexer: Lexer

  private var errors: ListBuffer[String] = ListBuffer()

  private var curToken: Token = None.orNull
  private var peekToken: Token = None.orNull

  def getErrors: List[String] = errors.toList

  protected def nextToken(): Unit = {
    curToken = peekToken
    peekToken = lexer.nextToken()
  }

  def parserProgram(): Program = {
    val statements: ListBuffer[Statement] = ListBuffer[Statement]()
    while (!curTokenIs(EOF)) {
      prepareStatement() match {
        case Some(value) => statements += value
        case None        => // no-ops
      }
      nextToken()
    }
    Program(statements = statements.toList)
  }

  private def prepareStatement(): Option[Statement] = curToken.`type` match {
    case LET => parseLetStatement()
    case _   => Option.empty
  }

  private def parseLetStatement(): Option[Statement] = {
    // let statement has below form
    // let <identifier> = <expression>;
    val letStmtToken = curToken
    if (!expectPeek(IDENT)) {
      Option.empty[Statement]
    } else {
      val ident = Identifier(token = curToken, value = curToken.literal)
      if (!expectPeek(ASSIGN)) {
        Option.empty[Statement]
      } else {
        while (!curTokenIs(SEMICOLON) && !curTokenIs(EOF)) {
          nextToken()
        }
        Some(LetStatement(token = letStmtToken, name = ident, value = None.orNull))
      }
    }
  }

  private def curTokenIs(tokenType: TokenType): Boolean =
    curToken.`type` == tokenType

  private def peekTokenIs(tokenType: TokenType): Boolean =
    peekToken.`type` == tokenType

  private def expectPeek(tokenType: TokenType): Boolean =
    if (peekTokenIs(tokenType)) {
      nextToken()
      true
    } else {
      peekError(tokenType = tokenType)
      false
    }

  private def peekError(tokenType: TokenType): Unit =
    errors += s"expected next token to be $tokenType, got ${peekToken.`type`} instead"

}

object Parser {

  def apply(_lexer: Lexer): Parser = new Parser() {
    override val lexer: Lexer = _lexer

    // Read two tokens, so curToken and peekToken are both set
    nextToken()
    nextToken()
  }

}
