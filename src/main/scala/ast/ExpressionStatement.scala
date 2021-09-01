// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.Token

case class ExpressionStatement(token: Token, expression: Expression) extends Statement {

  override def statementNode(): Unit = {}

  override def tokenLiteral(): String = token.literal

  override def toString: String =
    if (expression != null) {
      expression.toString
    } else {
      ""
    }

}
