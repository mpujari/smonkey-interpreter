// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.{Token, Tokens}

case class IntegerLiteral(token: Token, value: Int) extends Expression {
  require(token.`type` == Tokens.INT)

  override def expressionNode(): Unit = {}

  override def tokenLiteral(): String = token.literal

  override def toString: String = token.literal

}
