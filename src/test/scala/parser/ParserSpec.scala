// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package parser

import ast.{ExpressionStatement, Identifier, IntegerLiteral, LetStatement, Program, ReturnStatement, Statement}
import lexer.Lexer
import org.scalatest.FlatSpec
import token.Tokens

class ParserSpec extends FlatSpec {

  "test Let statement" should "parse program" in {
    val input: String =
      """
        let x = 5;
        let y = 10;
        let foobar = 838383;
        """.stripMargin
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)

    val program: Program = parser.parserProgram()
    assert(program.statements.size == 3)
    assert(parser.getErrors.isEmpty)
    val expectedIdentifiers: List[String] = List("x", "y", "foobar")

    expectedIdentifiers.zipWithIndex.foreach {
      case (ident, i) =>
        val statement: Statement = program.statements(i)
        assert(statement.isInstanceOf[LetStatement])
        val letStatement: LetStatement = statement.asInstanceOf[LetStatement]
        assert(letStatement.name.value == ident)
        assert(letStatement.name.tokenLiteral == ident)
    }

  }

  "simple test Let statement with no semicolon" should "parse program" in {
    val input: String =
      """
        let x = 5
        """.stripMargin
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)

    val program: Program = parser.parserProgram()
    assert(program.statements.size == 1)
    assert(parser.getErrors.isEmpty)
    val expectedIdentifiers: List[String] = List("x")

    expectedIdentifiers.zipWithIndex.foreach {
      case (ident, i) =>
        val statement: Statement = program.statements(i)
        assert(statement.isInstanceOf[LetStatement])
        val letStatement: LetStatement = statement.asInstanceOf[LetStatement]
        assert(letStatement.name.value == ident)
        assert(letStatement.name.tokenLiteral == ident)
    }

  }

  ignore should "invalid input have error message" in {
    val input: String =
      """
        let x 5;
        let = 10;
        let 838383
        """.stripMargin
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)

    val program: Program = parser.parserProgram()
    assert(program.statements.isEmpty)
    assert(parser.getErrors.nonEmpty)
    assert(
      parser.getErrors == List(
        "expected next token to be =, got INT instead",
        "expected next token to be IDENT, got = instead",
        "expected next token to be IDENT, got INT instead"
      )
    )
  }

  "empty input" should "have error message" in {
    val input =
      """
        """.stripMargin
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)
    val program: Program = parser.parserProgram()
    assert(parser.getErrors.isEmpty)
    assert(program.statements.isEmpty)
  }

  "return statement" should "parse program" in {
    val input =
      """
         return 5;
         return 10;
         return 993322;
        """.stripMargin
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)
    val program: Program = parser.parserProgram()
    assert(parser.getErrors.isEmpty)
    assert(program.statements.size == 3)

    program.statements.foreach { stmt =>
      assert(stmt.isInstanceOf[ReturnStatement])
      assert(stmt.asInstanceOf[ReturnStatement].tokenLiteral() == Tokens.RETURN)
    }
  }

  "simple identifier expression" should "pass the test" in {
    val input: String = "foobar;"
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)
    val program: Program = parser.parserProgram()
    assert(parser.getErrors.isEmpty)

    assert(program.statements.size == 1)
    val stmt: Statement = program.statements.head
    assert(stmt.isInstanceOf[ExpressionStatement])
    val expressionStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
    assert(expressionStmt.expression.isInstanceOf[Identifier])
    val identifier: Identifier = expressionStmt.expression.asInstanceOf[Identifier]
    assert(identifier.value == "foobar")
    assert(identifier.tokenLiteral() == "foobar")
  }

  "simple integer literals" should "pass the test" in {
    val input: String = "5;"
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)
    val program: Program = parser.parserProgram()
    assert(parser.getErrors.isEmpty)

    assert(program.statements.size == 1)
    val stmt: Statement = program.statements.head
    assert(stmt.isInstanceOf[ExpressionStatement])
    val expressionStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
    assert(expressionStmt.expression.isInstanceOf[IntegerLiteral])
    val integerLiteral: IntegerLiteral = expressionStmt.expression.asInstanceOf[IntegerLiteral]
    assert(integerLiteral.value == 5)
    assert(integerLiteral.tokenLiteral() == "5")
  }

}
