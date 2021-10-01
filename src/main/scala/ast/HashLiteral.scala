// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.{Token, Tokens}

case class HashLiteral(token: Token, pair: Map[Expression, Expression]) extends Expression {
  require(token.`type` == Tokens.LBRACE)

  override def expressionNode(): Unit = {}

  override def tokenLiteral(): String = token.literal

  override def toString: String = "{" + pair.map(p => s"${p._1.toString}:${p._2.toString}").mkString(", ") + "}"

}
