// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package aabstract

import ast._
import evaluator.Evaluator
import lexer.Lexer
import obj.NULL
import org.scalatest.FlatSpec
import parser.Parser

trait AbstractBaseSpec extends FlatSpec {

  def prepareEval(input: String): obj.Object = {
    val evaluated = Lexer(input)
    val parser = Parser(evaluated)
    val program: Program = parser.parserProgram()
    Evaluator.eval(program)
  }

  def testNullObject(o: obj.Object): Unit = assert(o == NULL)

  def testIntegerObject(o: obj.Object, expected: Int, errorMsg: Option[String] = None): Unit = {
    errorMsg.fold(assert(o.isInstanceOf[obj.Integer]))(e => assert(o.isInstanceOf[obj.Integer], e))
    val integer = o.asInstanceOf[obj.Integer]
    errorMsg.fold(assert(integer.value == expected))(e => assert(integer.value == expected, e))
  }

  def testBooleanObject(o: obj.Object, expected: Any): Unit = {
    assert(o.isInstanceOf[obj.Boolean])
    val b = o.asInstanceOf[obj.Boolean]
    assert(b.value == expected)
  }

  def testIntegerLiteral(expression: Expression, value: Int): Unit = {
    assert(expression.isInstanceOf[IntegerLiteral])
    val integerLiteral: IntegerLiteral = expression.asInstanceOf[IntegerLiteral]
    assert(integerLiteral.value == value)
  }

  def testLiteralExpression(exp: Expression, expected: Any): Unit = {
    val klass = Class.forName(expected.getClass.getName)
    if (klass.getName == classOf[Int].getName) {
      testIntegerLiteral(expression = exp, expected.asInstanceOf[Int])
    } else if (klass.getName == classOf[String].getName) {
      testIdentifier(expression = exp, expected.asInstanceOf[String])
    } else if (klass.getName == classOf[Boolean].getName) {
      testBooleanLiteral(expression = exp, expected.asInstanceOf[Boolean])
    }
  }

  def testIdentifier(expression: Expression, value: String): Unit = {
    assert(expression.isInstanceOf[Identifier])
    val ident = expression.asInstanceOf[Identifier]
    assert(ident.value == value)
    assert(ident.tokenLiteral() == value)
  }

  def testInfixExpression(expression: Expression, left: Any, operator: String, right: Any): Unit = {
    assert(expression.isInstanceOf[InfixExpression])
    val okExp = expression.asInstanceOf[InfixExpression]
    testLiteralExpression(okExp.left, left)
    assert(okExp.operator == operator)
    testLiteralExpression(okExp.right, right)
  }

  def testBooleanLiteral(expression: Expression, value: Boolean): Unit = {
    assert(expression.isInstanceOf[BooleanLiteral])
    val boolExp = expression.asInstanceOf[BooleanLiteral]
    assert(boolExp.value == value)
    assert(boolExp.tokenLiteral() == value.toString)
  }

}
