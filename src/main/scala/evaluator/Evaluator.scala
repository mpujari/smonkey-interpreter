// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package evaluator

import ast._
import obj.{FALSE, NULL, TRUE}

import scala.annotation.tailrec

object Evaluator {

  @tailrec
  def eval(node: Node): obj.Object =
    node match {
      case p: Program              => evalStatements(p.statements)
      case pe: PrefixExpression    => evalPrefixExpression(pe)
      case es: ExpressionStatement => eval(es.expression)
      case i: IntegerLiteral       => obj.Integer(i.value)
      case b: BooleanLiteral       => if (b.value) TRUE else FALSE
      case _                       => null
    }

  private def evalPrefixExpression(pe: PrefixExpression): obj.Object =
    pe.operator match {
      case "!" => evalBangOperatorExpression(eval(pe.right))
      case "-" => evalMinusOperatorExpression(eval(pe.right))
      case _   => NULL
    }

  private def evalMinusOperatorExpression(right: obj.Object): obj.Object = right match {
    case obj.Integer(value) => obj.Integer(-1 * value)
    case _                  => NULL
  }

  private def evalBangOperatorExpression(right: obj.Object): obj.Object =
    right match {
      case TRUE  => FALSE
      case FALSE => TRUE
      case NULL  => TRUE
      case _     => FALSE
    }

  private def evalStatements(statements: List[Statement]): obj.Object = {
    var result: obj.Object = NULL
    statements.foreach { s: Statement =>
      result = eval(s)
    }
    result
  }

}
