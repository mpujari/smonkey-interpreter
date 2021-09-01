// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.Token

case class Identifier(token: Token, value: String) extends Expression {

  def expressionNode(): Unit = {}

  def tokenLiteral(): String = token.literal

  override def toString: String = value

}
