// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.{Token, Tokens}

// Let statement is of the form let <identifier> = <expression>;
case class LetStatement(token: Token, name: Identifier, value: Expression) extends Statement {
  require(token.`type` == Tokens.LET)

  override def statementNode(): Unit = {}

  override def tokenLiteral(): String = token.literal

  override def toString: String = s"${token.literal} $name = ${if (value != null) { value.toString }};"

}
