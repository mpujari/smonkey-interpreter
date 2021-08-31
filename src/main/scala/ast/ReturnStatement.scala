// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.{Token, Tokens}

// return statement is of the form return <expression>;
case class ReturnStatement(token: Token, value: Expression) extends Statement {
  require(token.`type` == Tokens.RETURN)

  override def statementNode(): Unit = {}

  override def tokenLiteral(): String = token.literal

}
