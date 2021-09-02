// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package parser

import ast._
import lexer.Lexer
import org.scalatest.FlatSpec
import token.Tokens

class ParserSpec extends FlatSpec with AbstractBaseSpec {

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

  "simple prefix expression" should "pass the test" in {
    List(("!5;", "!", 5), ("-15;", "-", 15)) foreach { t =>
      val input = t._1
      val lexer: Lexer = Lexer(input)
      val parser: Parser = Parser(lexer)
      val program: Program = parser.parserProgram()
      assert(parser.getErrors.isEmpty)

      assert(program.statements.size == 1)
      val stmt: Statement = program.statements.head
      assert(stmt.isInstanceOf[ExpressionStatement])
      val expressionStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
      assert(expressionStmt.expression.isInstanceOf[PrefixExpression])
      val prefixExpression: PrefixExpression = expressionStmt.expression.asInstanceOf[PrefixExpression]
      assert(prefixExpression.operator == t._2)
      testIntegerLiteral(prefixExpression.right, t._3)
    }
  }

  "simple infex expression" should "pass the test" in {
    List(
      ("5 + 5;", 5, "+", 5),
      ("5 - 5;", 5, "-", 5),
      ("5 * 5;", 5, "*", 5),
      ("5 / 5;", 5, "/", 5),
      ("5 > 5;", 5, ">", 5),
      ("5 < 5;", 5, "<", 5),
      ("5 == 5;", 5, "==", 5),
      ("5 != 5;", 5, "!=", 5)
    ) foreach { t =>
      val input = t._1
      val lexer: Lexer = Lexer(input)
      val parser: Parser = Parser(lexer)
      val program: Program = parser.parserProgram()
      assert(parser.getErrors.isEmpty)

      assert(program.statements.size == 1)
      val stmt: Statement = program.statements.head
      assert(stmt.isInstanceOf[ExpressionStatement])
      val expressionStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
      assert(expressionStmt.expression.isInstanceOf[InfixExpression])
      val infixExpression: InfixExpression = expressionStmt.expression.asInstanceOf[InfixExpression]

      assert(infixExpression.operator == t._3)
      testIntegerLiteral(infixExpression.left, t._2)
      testIntegerLiteral(infixExpression.right, t._4)
    }
  }

  "operator precedence" should "pass the test" in {
    List(
      ("-a * b", "((-a) * b)"),
      ("!-a", "(!(-a))"),
      ("a + b +c", "((a + b) + c)"),
      ("a + b - c", "((a + b) - c)"),
      ("a * b * c", "((a * b) * c)"),
      ("a * b / c", "((a * b) / c)"),
      ("a + b / c", "(a + (b / c))"),
      ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
      ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
      ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
      ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
      ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
      ("-1 + 2", "((-1) + 2)"),
      ("true", "true"),
      ("false", "false"),
      ("3 > 5 == false", "((3 > 5) == false)"),
      ("3 < 5 == true", "((3 < 5) == true)")
    ) foreach { t =>
      val input = t._1
      val lexer: Lexer = Lexer(input)
      val parser: Parser = Parser(lexer)
      val program: Program = parser.parserProgram()
      assert(parser.getErrors.isEmpty)
      val a = program.toString
      assert(t._2 == a)
    }
  }

  "simple boolean literals" should "pass the test" in {
    List(
      ("true;", true),
      ("false;", false)
    ) foreach { t =>
      val input = t._1
      val lexer: Lexer = Lexer(input)
      val parser: Parser = Parser(lexer)
      val program: Program = parser.parserProgram()
      assert(parser.getErrors.isEmpty)

      assert(program.statements.size == 1)
      val stmt: Statement = program.statements.head
      assert(stmt.isInstanceOf[ExpressionStatement])
      val expressionStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
      assert(expressionStmt.expression.isInstanceOf[BooleanLiteral])
      val booleanLiteral: BooleanLiteral = expressionStmt.expression.asInstanceOf[BooleanLiteral]
      assert(booleanLiteral.value == t._2)
      assert(booleanLiteral.tokenLiteral() == t._2.toString)
    }
  }

  "parsing infix expression" should "pass test" in {
    List(
      ("true == true", true, "==", true),
      ("true != true", true, "!=", true),
      ("false == false", false, "==", false),
      ("false != false", false, "!=", false),
    ) foreach { t =>
      val input = t._1
      val lexer: Lexer = Lexer(input)
      val parser: Parser = Parser(lexer)
      val program: Program = parser.parserProgram()
      assert(parser.getErrors.isEmpty)

      assert(program.statements.size == 1)
      val stmt: Statement = program.statements.head
      assert(stmt.isInstanceOf[ExpressionStatement])
      val expressionStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
      assert(expressionStmt.expression.isInstanceOf[InfixExpression])
      val infixExpression: InfixExpression = expressionStmt.expression.asInstanceOf[InfixExpression]
      testInfixExpression(infixExpression, t._2, t._3, t._4)
    }
  }

  "parsing prefix expression" should "pass test" in {
    List(
      ("!true", "!", true),
      ("!false", "!", false)
    ) foreach { t =>
      val input = t._1
      val lexer: Lexer = Lexer(input)
      val parser: Parser = Parser(lexer)
      val program: Program = parser.parserProgram()
      assert(parser.getErrors.isEmpty)

      assert(program.statements.size == 1)
      val stmt: Statement = program.statements.head
      assert(stmt.isInstanceOf[ExpressionStatement])
      val expressionStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
      assert(expressionStmt.expression.isInstanceOf[PrefixExpression])

      val prefixExpression: PrefixExpression = expressionStmt.expression.asInstanceOf[PrefixExpression]
      assert(prefixExpression.operator == t._2)

      testBooleanLiteral(prefixExpression.right, t._3)
    }
  }

  "parsing operator precedence expression" should "pass test" in {
    List(
      ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
      ("(5 + 5) * 2", "((5 + 5) * 2)"),
      ("2 / (5 + 5)", "(2 / (5 + 5))"),
      ("-(5 + 5)", "(-(5 + 5))"),
      ("!(true == true)", "(!(true == true))")
    ) foreach { t =>
      val input = t._1
      val lexer: Lexer = Lexer(input)
      val parser: Parser = Parser(lexer)
      val program: Program = parser.parserProgram()
      assert(parser.getErrors.isEmpty)
      val a = program.toString
      assert(t._2 == a)
    }
  }

  "parsing if expression" should "pass test" in {
    val input = "if (x < y) { x }"
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)
    val program: Program = parser.parserProgram()
    assert(parser.getErrors.isEmpty)
    assert(program.statements.size == 1)
    val stmt: Statement = program.statements.head
    assert(stmt.isInstanceOf[ExpressionStatement])
    val expStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
    assert(expStmt.expression.isInstanceOf[IfExpression])
    val exp = expStmt.expression.asInstanceOf[IfExpression]
    testInfixExpression(exp.condition, "x", "<", "y")

    assert(exp.consequence.statements.size == 1)
    assert(exp.consequence.statements.head.isInstanceOf[ExpressionStatement])
    val condExp = exp.consequence.statements.head.asInstanceOf[ExpressionStatement]

    testIdentifier(condExp.expression, "x")
    assert(exp.alternative == null)
  }

  "parsing if-else expression" should "pass test" in {
    val input = "if (x < y) { x } else { y }"
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)
    val program: Program = parser.parserProgram()
    assert(parser.getErrors.isEmpty)
    assert(program.statements.size == 1)
    val stmt: Statement = program.statements.head
    assert(stmt.isInstanceOf[ExpressionStatement])
    val expStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
    assert(expStmt.expression.isInstanceOf[IfExpression])
    val exp = expStmt.expression.asInstanceOf[IfExpression]
    testInfixExpression(exp.condition, "x", "<", "y")

    assert(exp.consequence.statements.size == 1)
    assert(exp.consequence.statements.head.isInstanceOf[ExpressionStatement])
    val condExp = exp.consequence.statements.head.asInstanceOf[ExpressionStatement]

    testIdentifier(condExp.expression, "x")
    assert(exp.alternative.statements.size == 1)
    assert(exp.alternative.statements.head.isInstanceOf[ExpressionStatement])
    testIdentifier(exp.alternative.statements.head.asInstanceOf[ExpressionStatement].expression, "y")
  }

}
