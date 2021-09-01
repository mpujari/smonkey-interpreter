// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package parser

import ast.{Expression, IntegerLiteral}

trait AbstractBaseSpec {

  def testIntegerLiteral(expression: Expression, value: Int): Unit = {
    assert(expression.isInstanceOf[IntegerLiteral])
    val integerLiteral: IntegerLiteral = expression.asInstanceOf[IntegerLiteral]
    assert(integerLiteral.value == value)
  }

}
