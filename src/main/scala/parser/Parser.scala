// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package parser

import ast.{Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, Program, ReturnStatement, Statement}
import lexer.Lexer
import token.Token
import token.Tokens._
import token.Precedences._

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

trait Parser {

  val lexer: Lexer

  private var errors: ListBuffer[String] = ListBuffer()

  private var curToken: Token = None.orNull
  private var peekToken: Token = None.orNull

  private val prefixParseFns: Map[TokenType, () => Option[Expression]] = Map(
    IDENT -> parseIdentifier,
    INT -> parseIntegerLiteral
  )

  private val infixParseFns: Map[TokenType, (Expression) => Expression] = Map()

  private def parseIdentifier: () => Option[Identifier] = { () =>
    Some(Identifier(curToken, value = curToken.literal))
  }

  private def parseIntegerLiteral: () => Option[Expression] = { () =>
    val token = curToken
    Try(token.literal.toInt) match {
      case Failure(e) =>
        errors += s"could not parse ${curToken.literal} as integer"
        Option.empty[Expression]
      case Success(i) => Some(IntegerLiteral(token = token, value = i))
    }
  }

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
    case LET    => parseLetStatement()
    case RETURN => parseReturnStatement()
    case _      => parseExpressionStatement()
  }

  private def parseExpressionStatement(): Option[ExpressionStatement] = {
    val token = curToken
    val expression = parseExpression(LOWEST.id)
    if (peekTokenIs(SEMICOLON)) {
      nextToken()
    }
    Some(ExpressionStatement(token = token, expression = expression.orNull)) // TODO revisit orNull
  }

  private def parseExpression(precedence: Int): Option[Expression] =
    prefixParseFns.get(curToken.`type`).flatMap(f => f())

  private def parseReturnStatement(): Option[Statement] = {
    val token = curToken
    nextToken()
    // TODO pending with expression parsing
    while (!curTokenIs(SEMICOLON) && !curTokenIs(EOF)) {
      nextToken()
    }
    Some(ReturnStatement(token = token, returnValue = None.orNull))
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
