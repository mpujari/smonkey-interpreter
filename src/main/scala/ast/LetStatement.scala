// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.Token

case class LetStatement(token: Token, name: Identifier, value: Expression) extends Statement {

  override def statementNode(): Unit = {}

  override def tokenLiteral(): String = token.literal

}
