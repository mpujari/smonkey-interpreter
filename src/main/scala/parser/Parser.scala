// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package parser

import ast._
import lexer.Lexer
import token.Precedences._
import token.Token
import token.Tokens._

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

trait Parser {

  val lexer: Lexer

  private val errors: ListBuffer[String] = ListBuffer()

  private var curToken: Token = None.orNull
  private var peekToken: Token = None.orNull

  private val precedences: Map[TokenType, Int] = Map(
    EQ -> EQUALS.id,
    NOT_EQ -> EQUALS.id,
    LT -> LESS_GREATER.id,
    GT -> LESS_GREATER.id,
    PLUS -> SUM.id,
    MINUS -> SUM.id,
    SLASH -> PRODUCT.id,
    ASTERISK -> PRODUCT.id
  )
  private val prefixParseFns: Map[TokenType, () => Option[Expression]] = Map(
    IDENT -> parseIdentifier,
    INT -> parseIntegerLiteral,
    BANG -> parsePrefixExpression,
    MINUS -> parsePrefixExpression
  )

  private val infixParseFns: Map[TokenType, Expression => Option[Expression]] = Map(
    PLUS -> parseInfixExpression,
    MINUS -> parseInfixExpression,
    SLASH -> parseInfixExpression,
    ASTERISK -> parseInfixExpression,
    EQ -> parseInfixExpression,
    NOT_EQ -> parseInfixExpression,
    LT -> parseInfixExpression,
    GT -> parseInfixExpression
  )

  private def parseInfixExpression: Expression => Option[Expression] = { (left: Expression) =>
    val token = curToken
    val cu = curPrecedences()
    nextToken()
    Some(InfixExpression(token = token, operator = token.literal, left = left, right = parseExpression(cu).orNull)) // TODO orNull revisit
  }

  private def parseIdentifier: () => Option[Identifier] = { () =>
    Some(Identifier(curToken, value = curToken.literal))
  }

  private def parseIntegerLiteral: () => Option[Expression] = { () =>
    val token = curToken
    Try(token.literal.toInt) match {
      case Failure(_) =>
        errors += s"could not parse ${curToken.literal} as integer"
        Option.empty[Expression]
      case Success(i) => Some(IntegerLiteral(token = token, value = i))
    }
  }

  private def parsePrefixExpression: () => Option[Expression] = { () =>
    val token = curToken
    nextToken()
    Some(PrefixExpression(token = token, operator = token.literal, right = parseExpression(PREFIX.id).orNull)) // TODO orNull check
  }

  private def peekPrecedences(): Int = precedences.getOrElse(peekToken.`type`, LOWEST.id)

  private def curPrecedences(): Int = precedences.getOrElse(curToken.`type`, LOWEST.id)

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
    prefixParseFns.get(curToken.`type`) match {
      case Some(prefixFn) =>
        var leftExp = prefixFn()
        while (!peekTokenIs(SEMICOLON) && precedence < peekPrecedences()) {
          val infixOpt = infixParseFns.get(peekToken.`type`)
          if (infixOpt.isEmpty) {
            return leftExp
          }
          nextToken()
          leftExp = infixOpt.get(leftExp.get)
        }
        leftExp
      case None =>
        noPrefixParseFnError(curToken.`type`)
        Option.empty[Expression]
    }

  private def noPrefixParseFnError(tokenType: TokenType): Unit =
    errors += s"no prefix parse function for $tokenType found"

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
