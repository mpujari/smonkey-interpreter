// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package parser

import ast.{LetStatement, Statement}
import lexer.Lexer
import org.scalatest.FlatSpec

class ParserSpec extends FlatSpec {

  "test Let statement" should "parse program" in {
    val input =
      """
        let x = 5;
        let y = 10;
        let foobar = 838383;
        """.stripMargin
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)

    val program = parser.parserProgram()
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
    val input =
      """
        let x = 5
        """.stripMargin
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)

    val program = parser.parserProgram()
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

  "invalid input" should "have error message" in {
    val input =
      """
        let x 5;
        let = 10;
        let 838383
        """.stripMargin
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)

    val program = parser.parserProgram()
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

    val program = parser.parserProgram()
    assert(program.statements.isEmpty)
    assert(parser.getErrors.isEmpty)
  }

}
