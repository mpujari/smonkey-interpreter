// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.{Token, Tokens}

case class ArrayLiteral(token: Token, elements: List[Expression]) extends Expression {
  require(token.`type` == Tokens.LBRACKET)

  override def expressionNode(): Unit = {}

  override def tokenLiteral(): String = token.literal

  override def toString: String = "[" + elements.map(_.toString).mkString(", ") + "]"

}
