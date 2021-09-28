// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.{Token, Tokens}

case class IndexExpression(token: Token, left: Expression, index: Expression) extends Expression {
  require(token.`type` == Tokens.LBRACKET)

  override def expressionNode(): Unit = {}

  override def tokenLiteral(): String = token.literal

  override def toString: String = s"(${left.toString}[${index.toString}])"

}
