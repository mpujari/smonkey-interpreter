// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import token.Token

// if (<condition>) <consequence> else <alternative>
case class IfExpression(token: Token, condition: Expression, consequence: BlockStatement, alternative: BlockStatement)
  extends Expression {

  override def expressionNode(): Unit = {}

  override def tokenLiteral(): String = token.literal

  override def toString: String = {
    val altStr = if (alternative != null) { "else " + alternative.toString } else { "" }
    s"if ${condition.toString} ${consequence.toString}" + altStr
  }

}
