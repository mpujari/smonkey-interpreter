// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.Token

case class BlockStatement(token: Token, statements: List[Statement] = List()) extends Statement {

  override def statementNode(): Unit = {}

  override def tokenLiteral(): String = token.literal

  override def toString: String = statements.map(_.toString).mkString("")

}
