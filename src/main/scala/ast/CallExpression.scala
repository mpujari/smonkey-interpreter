// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.Token

case class CallExpression(token: Token, function: Expression, arguments: List[Expression]) extends Expression {
  require(token.`type` == "(")

  override def expressionNode(): Unit = {}

  override def tokenLiteral(): String = token.literal

  override def toString: String = s"${function.toString}(${arguments.map(_.toString).mkString(", ")})"

}
