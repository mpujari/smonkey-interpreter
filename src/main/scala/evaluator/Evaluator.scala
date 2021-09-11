// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package evaluator

import ast._
import obj.{ERROR_OBJ, FALSE, INTEGER_OBJ, NULL, Return, TRUE}

object Evaluator {

  def eval(node: Node): obj.Object =
    node match {
      case p: Program => evalProgram(p.statements)
      case rs: ReturnStatement =>
        val returnValue = eval(rs.returnValue)
        if (isError(returnValue)) {
          return returnValue
        }
        obj.Return(returnValue)
      case pe: PrefixExpression =>
        val e = eval(pe.right)
        if (isError(e)) {
          return e
        }
        evalPrefixExpression(pe.operator, e)
      case ie: InfixExpression =>
        val evalLeft = eval(ie.left)
        if (isError(evalLeft)) {
          return evalLeft
        }
        val evalRight = eval(ie.right)
        if (isError(evalRight)) {
          return evalRight
        }
        evalInfixExpression(ie.operator, evalLeft, evalRight)
      case es: ExpressionStatement => eval(es.expression)
      case ife: IfExpression       => evalIfExpression(ife)
      case bs: BlockStatement      => evalBlockStatement(bs)
      case i: IntegerLiteral       => obj.Integer(i.value)
      case b: BooleanLiteral       => nativeBoolToBooleanObj(b.value)

      case _ => null
    }

  private def isError(o: obj.Object): Boolean =
    if (o != null) {
      o.`type`() == ERROR_OBJ
    } else {
      false
    }

  private def nativeBoolToBooleanObj(b: Boolean): obj.Boolean = if (b) TRUE else FALSE

  private def evalInfixExpression(operator: String, left: obj.Object, right: obj.Object): obj.Object =
    (operator, left, right) match {
      case (_, _: obj.Integer, _: obj.Integer)   => evalIntegerInfixExpression(operator, left, right)
      case ("==", _, _)                          => nativeBoolToBooleanObj(left == right)
      case ("!=", _, _)                          => nativeBoolToBooleanObj(left != right)
      case (o, l, r) if l.`type`() != r.`type`() => obj.Error(s"type mismatch: ${l.`type`()} $o ${r.`type`()}")
      case (o, l, r)                             => obj.Error(s"unknown operator: ${l.`type`()} $o ${r.`type`()}")
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

  private def evalPrefixExpression(operator: String, right: obj.Object): obj.Object =
    operator match {
      case "!" => evalBangOperator(right)
      case "-" => evalMinusPrefixOperator(right)
      case _   => obj.Error(s"unknown operator: $operator${right.`type`()}")
    }

  private def evalMinusPrefixOperator(right: obj.Object): obj.Object = right match {
    case obj.Integer(value)             => obj.Integer(-1 * value)
    case r if r.`type`() != INTEGER_OBJ => obj.Error(s"unknown operator: -${r.`type`()}")
    case _                              => NULL
  }

  private def evalBangOperator(right: obj.Object): obj.Object =
    right match {
      case TRUE  => FALSE
      case FALSE => TRUE
      case NULL  => TRUE
      case _     => FALSE
    }

  private def evalIfExpression(ife: IfExpression): obj.Object = {
    val e = eval(ife.condition)
    if (isError(e)) {
      return e
    }
    if (isTruth(e)) {
      eval(ife.consequence)
    } else if (ife.alternative != null) {
      eval(ife.alternative)
    } else {
      NULL
    }
  }

  private def isTruth(o: obj.Object): scala.Boolean =
    o match {
      case NULL  => false
      case TRUE  => true
      case FALSE => false
      case _     => true
    }

  private def evalBlockStatement(blockStatement: BlockStatement): obj.Object = {
    var result: obj.Object = NULL
    blockStatement.statements.foreach { s: Statement =>
      result = eval(s)
      result match {
        case value: Return => return value
        case e: obj.Error  => return e
        case _             => // no-ops
      }
    }
    result
  }

  private def evalProgram(statements: List[Statement]): obj.Object = {
    var result: obj.Object = NULL
    statements.foreach { s: Statement =>
      result = eval(s)
      result match {
        case r: Return    => return r.returnValue
        case e: obj.Error => return e
        case _            => // no-ops
      }
    }
    result
  }

}
