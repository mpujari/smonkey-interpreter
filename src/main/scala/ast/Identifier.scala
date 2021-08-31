// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.Token

case class Identifier(token: Token, value: String) {

  def expressionNode(): Unit = {}

  def tokenLiteral: String = token.literal

}
