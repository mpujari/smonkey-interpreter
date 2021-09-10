// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package evaluator

import ast._
import obj.{FALSE, NULL, TRUE}

object Evaluator {

  def eval(node: Node): obj.Object =
    node match {
      case p: Program           => evalStatements(p.statements)
      case pe: PrefixExpression => evalPrefixExpression(pe)
      case ie: InfixExpression =>
        val evalLeft = eval(ie.left)
        val evalRight = eval(ie.right)
        evalInfixExpression(ie.operator, evalLeft, evalRight)
      case es: ExpressionStatement => eval(es.expression)
      case i: IntegerLiteral       => obj.Integer(i.value)
      case b: BooleanLiteral       => nativeBoolToBooleanObj(b.value)
      case _                       => null
    }

  private def nativeBoolToBooleanObj(b: Boolean): obj.Boolean = if (b) TRUE else FALSE

  private def evalInfixExpression(operator: String, left: obj.Object, right: obj.Object): obj.Object =
    (operator, left, right) match {
      case (_, _: obj.Integer, _: obj.Integer) => evalIntegerInfixExpression(operator, left, right)
      case ("==", _, _)                        => nativeBoolToBooleanObj(left == right)
      case ("!=", _, _)                        => nativeBoolToBooleanObj(left != right)
      case _                                   => NULL
    }

  private def evalIntegerInfixExpression(operator: String, left: obj.Object, right: obj.Object): obj.Object = {
    val leftValue = left.asInstanceOf[obj.Integer].value
    val rightValue = right.asInstanceOf[obj.Integer].value
    operator match {
      case "+"  => obj.Integer(leftValue + rightValue)
      case "-"  => obj.Integer(leftValue - rightValue)
      case "*"  => obj.Integer(leftValue * rightValue)
      case "/"  => obj.Integer(leftValue / rightValue)
      case ">"  => nativeBoolToBooleanObj(leftValue > rightValue)
      case "<"  => nativeBoolToBooleanObj(leftValue < rightValue)
      case "==" => nativeBoolToBooleanObj(leftValue == rightValue)
      case "!=" => nativeBoolToBooleanObj(leftValue != rightValue)
      case "<=" => nativeBoolToBooleanObj(leftValue <= rightValue)
      case ">=" => nativeBoolToBooleanObj(leftValue >= rightValue)
      case _    => NULL
    }
  }

  private def evalPrefixExpression(pe: PrefixExpression): obj.Object =
    pe.operator match {
      case "!" => evalBangOperator(eval(pe.right))
      case "-" => evalMinusPrefixOperator(eval(pe.right))
      case _   => NULL
    }

  private def evalMinusPrefixOperator(right: obj.Object): obj.Object = right match {
    case obj.Integer(value) => obj.Integer(-1 * value)
    case _                  => NULL
  }

  private def evalBangOperator(right: obj.Object): obj.Object =
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
