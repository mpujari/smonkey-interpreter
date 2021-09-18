// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package aabstract

import ast._
import evaluator.Evaluator
import lexer.Lexer
import obj.{Environment, NULL}
import org.scalatest.FlatSpec
import parser.Parser

trait AbstractBaseSpec extends FlatSpec {

  def prepareEval(input: String): obj.Object = {
    val evaluated = Lexer(input)
    val parser = Parser(evaluated)
    val program: Program = parser.parserProgram()
    implicit val env: Environment = Environment()
    Evaluator.eval(program)
  }

  def testNullObject(o: obj.Object): Unit = assert(o == NULL)

  def testIntegerObject(o: obj.Object, expected: Int, errorMsg: Option[String] = None): Unit = {
    errorMsg.fold(assert(o.isInstanceOf[obj.Integer]))(e => assert(o.isInstanceOf[obj.Integer], e))
    val integer = o.asInstanceOf[obj.Integer]
    errorMsg.fold(assert(integer.value == expected))(e => assert(integer.value == expected, e))
  }

  def testStringObject(o: obj.Object, expected: String, errorMsg: Option[String] = None): Unit = {
    errorMsg.fold(assert(o.isInstanceOf[obj.SString]))(e => assert(o.isInstanceOf[obj.SString], e))
    val string = o.asInstanceOf[obj.SString]
    errorMsg.fold(assert(string.value == expected))(e => assert(string.value == expected, e))
  }

  def testFloatObject(o: obj.Object, expected: Float, errorMsg: Option[String] = None): Unit = {
    errorMsg.fold(assert(o.isInstanceOf[obj.Float]))(e => assert(o.isInstanceOf[obj.Float], e))
    val f = o.asInstanceOf[obj.Float]
    errorMsg.fold(assert(f.value == expected))(e => assert(f.value == expected, e))
  }

  def testBooleanObject(o: obj.Object, expected: Any, errorMsg: Option[String] = None): Unit = {
    assert(o.isInstanceOf[obj.Boolean])
    val b = o.asInstanceOf[obj.Boolean]
    errorMsg.fold(assert(b.value == expected))(e => assert(b.value == expected, e))
  }

  def testIntegerLiteral(expression: Expression, value: Int): Unit = {
    assert(expression.isInstanceOf[IntegerLiteral])
    val integerLiteral: IntegerLiteral = expression.asInstanceOf[IntegerLiteral]
    assert(integerLiteral.value == value)
  }

  def testFloatLiteral(expression: Expression, value: Float): Unit = {
    assert(expression.isInstanceOf[FloatLiteral])
    val floatLiteral: FloatLiteral = expression.asInstanceOf[FloatLiteral]
    assert(floatLiteral.value.compareTo(value) == 0)
  }

  def testLiteralExpression(exp: Expression, expected: Any): Unit = {
    val klass = Class.forName(expected.getClass.getName)
    if (klass.getName == classOf[Float].getName || klass.getName == classOf[java.lang.Float].getName) {
      testFloatLiteral(expression = exp, expected.asInstanceOf[Float])
    } else if (klass.getName == classOf[Int].getName || klass.getName == classOf[java.lang.Integer].getName) {
      testIntegerLiteral(expression = exp, expected.asInstanceOf[Int])
    } else if (klass.getName == classOf[String].getName) {
      testIdentifier(expression = exp, expected.asInstanceOf[String])
    } else if (klass.getName == classOf[Boolean].getName || klass.getName == classOf[java.lang.Boolean].getName) {
      testBooleanLiteral(expression = exp, expected.asInstanceOf[Boolean])
    } else {
      fail(s"type '$klass' not handled")
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
