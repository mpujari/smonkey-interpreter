// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.Token

case class InfixExpression(token: Token, left: Expression, operator: String, right: Expression) extends Expression {

  override def expressionNode(): Unit = {}

  override def tokenLiteral(): String = token.literal

  override def toString: String = s"(${left.toString} $operator ${right.toString})"

}
