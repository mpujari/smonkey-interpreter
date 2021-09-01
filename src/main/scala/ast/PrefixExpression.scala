// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.Token

// <prefix operator><expression>;
case class PrefixExpression(token: Token, operator: String, right: Expression) extends Expression {

  override def expressionNode(): Unit = {}

  override def tokenLiteral(): String = token.literal

  override def toString: String = s"($operator${right.toString})"

}
