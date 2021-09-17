// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package parser

import aabstract.AbstractBaseSpec
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
        let a = 10.10;
        """.stripMargin
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)

    val program: Program = parser.parserProgram()
    assert(program.statements.size == 4)
    assert(parser.getErrors.isEmpty)
    val expectedIdentifiers: List[String] = List("x", "y", "foobar", "a")

    expectedIdentifiers.zipWithIndex.foreach {
      case (ident, i) =>
        val statement: Statement = program.statements(i)
        assert(statement.isInstanceOf[LetStatement])
        val letStatement: LetStatement = statement.asInstanceOf[LetStatement]
        assert(letStatement.name.value == ident)
        assert(letStatement.name.tokenLiteral == ident)
    }
  }

  "test Let statement some more" should "parse program" in {
    List(
      ("let x = 5;", "x", 5),
      ("let x = 5.2566;", "x", 5.2566f),
      ("let y = true;", "y", true),
      ("let foobar = y;", "foobar", "y")
    ) foreach { t =>
      val input: String = t._1
      val lexer: Lexer = Lexer(input)
      val parser: Parser = Parser(lexer)

      val program: Program = parser.parserProgram()
      assert(program.statements.size == 1)
      assert(parser.getErrors.isEmpty)
      val statement: Statement = program.statements.head
      assert(statement.isInstanceOf[LetStatement])
      // TODO testLetStatement(statement, t._2)
      val letStatement: LetStatement = statement.asInstanceOf[LetStatement]

      testLiteralExpression(letStatement.value, t._3)
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

  "invalid input" should "have error message" in {
    List(
      "let x 5;" -> List("expected next token to be =, got INT instead"),
      "let x 5.10;" -> List("expected next token to be =, got FLOAT instead"),
      "let = 10;" -> List("expected next token to be IDENT, got = instead", "no prefix parse function for = found"),
      "let 838383;" -> List("expected next token to be IDENT, got INT instead")
    ).foreach { t =>
      val input: String = t._1
      val lexer: Lexer = Lexer(input)
      val parser: Parser = Parser(lexer)
      parser.parserProgram()
      assert(parser.getErrors.nonEmpty)
      assert(t._2 == parser.getErrors)
    }
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
         return 25.01;
        """.stripMargin
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)
    val program: Program = parser.parserProgram()
    assert(parser.getErrors.isEmpty)
    assert(program.statements.size == 4)

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

  "simple float literals" should "pass the test" in {
    val input: String = "5.01;"
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)
    val program: Program = parser.parserProgram()
    assert(parser.getErrors.isEmpty)

    assert(program.statements.size == 1)
    val stmt: Statement = program.statements.head
    assert(stmt.isInstanceOf[ExpressionStatement])
    val expressionStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
    assert(expressionStmt.expression.isInstanceOf[FloatLiteral])
    val floatLiteral: FloatLiteral = expressionStmt.expression.asInstanceOf[FloatLiteral]
    assert(floatLiteral.value.compareTo(5.01f) == 0)
    assert(floatLiteral.tokenLiteral() == "5.01")
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

  "simple prefix expression with Float" should "pass the test" in {
    List(("-0.15;", "-", 0.15f)) foreach { t =>
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
      testFloatLiteral(prefixExpression.right, t._3)
    }
  }

  "simple infix expression" should "pass the test" in {
    List(
      ("5 + 5;", 5, "+", 5),
      ("5 - 5;", 5, "-", 5),
      ("5 * 5;", 5, "*", 5),
      ("5 / 5;", 5, "/", 5),
      ("5 > 5;", 5, ">", 5),
      ("5 < 5;", 5, "<", 5),
      ("5 == 5;", 5, "==", 5),
      ("5 != 5;", 5, "!=", 5),
      ("5 <= 5;", 5, "<=", 5),
      ("5 >= 5;", 5, ">=", 5)
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

  "simple infix expression with Float" should "pass the test" in {
    List(
      ("5.001 + 5.001;", 5.001f, "+", 5.001f),
      ("5.001 - 5.001;", 5.001f, "-", 5.001f),
      ("5.001 * 5.001;", 5.001f, "*", 5.001f),
      ("5.001 / 5.001;", 5.001f, "/", 5.001f),
      ("5.001 > 5.001;", 5.001f, ">", 5.001f),
      ("5.001 < 5.001;", 5.001f, "<", 5.001f),
      ("5.001 == 5.001;", 5.001f, "==", 5.001f),
      ("5.001 != 5.001;", 5.001f, "!=", 5.001f),
      ("5.001 <= 5.001;", 5.001f, "<=", 5.001f),
      ("5.001 >= 5.001;", 5.001f, ">=", 5.001f)
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
      testFloatLiteral(infixExpression.left, t._2)
      testFloatLiteral(infixExpression.right, t._4)
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
      ("0.3 + 0.4; -0.5 * 0.5", "(0.3 + 0.4)((-0.5) * 0.5)"),
      ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
      ("0.5 > 0.4 == 0.3 < 0.4", "((0.5 > 0.4) == (0.3 < 0.4))"),
      ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
      ("5.01 < 4.01 != 3.01 > 4.05", "((5.01 < 4.01) != (3.01 > 4.05))"),
      ("-0.3 + -.04 * 5 == 3 * 1 + 4 * 5", "(((-0.3) + ((-.04) * 5)) == ((3 * 1) + (4 * 5)))"),
      ("-1 + 2", "((-1) + 2)"),
      ("-0.1 + 2", "((-0.1) + 2)"),
      ("true", "true"),
      ("false", "false"),
      ("3 > 5 == false", "((3 > 5) == false)"),
      ("-0.3 > 5 == false", "(((-0.3) > 5) == false)"),
      ("0.3 > 5 == false", "((0.3 > 5) == false)"),
      ("3 < 5 == true", "((3 < 5) == true)"),
      ("3 < 0.5 == true", "((3 < 0.5) == true)"),
      ("3 < -0.5 == true", "((3 < (-0.5)) == true)")
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
      ("0.1 + (0.2 + 3) + 4", "((0.1 + (0.2 + 3)) + 4)"),
      ("(5 + 5) * 2", "((5 + 5) * 2)"),
      ("(-5 + 5) * 2", "(((-5) + 5) * 2)"),
      ("(-0.5 + 5) * -0.2", "(((-0.5) + 5) * (-0.2))"),
      ("2 / (5 + 5)", "(2 / (5 + 5))"),
      ("0.2 / (0.5 + 0.5)", "(0.2 / (0.5 + 0.5))"),
      ("-0.2 / (-0.5 + -0.5)", "((-0.2) / ((-0.5) + (-0.5)))"),
      ("-(5 + 5)", "(-(5 + 5))"),
      ("-(0.5 + 0.5)", "(-(0.5 + 0.5))"),
      ("-(-0.5 + -0.5)", "(-((-0.5) + (-0.5)))"),
      ("!(true == true)", "(!(true == true))"),
      ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
      ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
      ("add(a, b, 0.1, 0.2 * 0.3, 0.4 + 5, add(0.6, 7 * 0.8))",
       "add(a, b, 0.1, (0.2 * 0.3), (0.4 + 5), add(0.6, (7 * 0.8)))"),
      ("add(a, b, -0.1, 0.2 * -0.3, -0.4 + 5, add(0.6, 7 * -0.8))",
       "add(a, b, (-0.1), (0.2 * (-0.3)), ((-0.4) + 5), add(0.6, (7 * (-0.8))))"),
      ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))")
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

  "parsing function literal" should "pass test" in {
    val input = "fn(x, y) { x + y; }"

    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)
    val program: Program = parser.parserProgram()
    assert(parser.getErrors.isEmpty)
    assert(program.statements.size == 1)
    val stmt: Statement = program.statements.head
    assert(stmt.isInstanceOf[ExpressionStatement])
    val expStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
    assert(expStmt.expression.isInstanceOf[FunctionLiteral])
    val fn = expStmt.expression.asInstanceOf[FunctionLiteral]
    assert(fn.parameters.size == 2)
    testLiteralExpression(fn.parameters.head, "x")
    testLiteralExpression(fn.parameters(1), "y")

    assert(fn.body.statements.size == 1)
    assert(fn.body.statements.head.isInstanceOf[ExpressionStatement])

    val bodyExpStmt = fn.body.statements.head.asInstanceOf[ExpressionStatement]
    testInfixExpression(bodyExpStmt.expression, "x", "+", "y")
  }

  "parsing function parameter parsing" should "pass test" in {
    List(
      ("fn() {};", List[String]()),
      ("fn(x) {};", List[String]("x")),
      ("fn(x, y, z) {};", List[String]("x", "y", "z"))
    ) foreach { t =>
      val input = t._1
      val lexer: Lexer = Lexer(input)
      val parser: Parser = Parser(lexer)
      val program: Program = parser.parserProgram()
      assert(parser.getErrors.isEmpty)
      assert(program.statements.size == 1)
      val stmt: Statement = program.statements.head
      assert(stmt.isInstanceOf[ExpressionStatement])
      val expStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
      assert(expStmt.expression.isInstanceOf[FunctionLiteral])
      val fn = expStmt.expression.asInstanceOf[FunctionLiteral]
      assert(fn.parameters.map(_.token.literal) == t._2)
    }
  }

  "parsing call expression" should "pass test" in {
    val input = "add(1, 2 * 3, 4 + 5);"
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)
    val program: Program = parser.parserProgram()
    assert(parser.getErrors.isEmpty)
    assert(program.statements.size == 1)
    val stmt: Statement = program.statements.head
    assert(stmt.isInstanceOf[ExpressionStatement])
    val expStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
    assert(expStmt.expression.isInstanceOf[CallExpression])
    val callExp = expStmt.expression.asInstanceOf[CallExpression]
    testIdentifier(callExp.function, "add")
    assert(callExp.arguments.size == 3)
    testLiteralExpression(callExp.arguments.head, 1)
    testInfixExpression(callExp.arguments(1), 2, "*", 3)
    testInfixExpression(callExp.arguments(2), 4, "+", 5)
  }

  "parsing call expression with float args" should "pass test" in {
    val input = "add(0.1, 0.2 * 0.3, 0.4 + 0.5);"
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)
    val program: Program = parser.parserProgram()
    assert(parser.getErrors.isEmpty)
    assert(program.statements.size == 1)
    val stmt: Statement = program.statements.head
    assert(stmt.isInstanceOf[ExpressionStatement])
    val expStmt: ExpressionStatement = stmt.asInstanceOf[ExpressionStatement]
    assert(expStmt.expression.isInstanceOf[CallExpression])
    val callExp = expStmt.expression.asInstanceOf[CallExpression]
    testIdentifier(callExp.function, "add")
    assert(callExp.arguments.size == 3)
    testLiteralExpression(callExp.arguments.head, 0.1f)
    testInfixExpression(callExp.arguments(1), 0.2f, "*", 0.3f)
    testInfixExpression(callExp.arguments(2), 0.4f, "+", 0.5f)
  }

}
