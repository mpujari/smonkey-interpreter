// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package evaluator

import ast._
import obj.{ERROR_OBJ, FALSE, INTEGER_OBJ, NULL, Return, TRUE}
import obj._

import scala.collection.mutable.ListBuffer

object Evaluator {

  def eval(node: Node)(implicit env: Environment): obj.Object =
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
      case f: FloatLiteral         => obj.Float(f.value)
      case b: BooleanLiteral       => nativeBoolToBooleanObj(b.value)
      case i: Identifier           => evalIdentifier(i)
      case ls: LetStatement =>
        val v = eval(ls.value)
        if (isError(v)) {
          v
        } else {
          env.set(ls.name.value, v)
          v
        }
      case fn: FunctionLiteral =>
        obj.Function(fn.parameters, body = fn.body, env = env)
      case c: CallExpression =>
        val e = eval(c.function)
        if (isError(e)) {
          return e
        }
        val args = evalExpressions(c.arguments)
        if (args.length == 1 && isError(args.head)) {
          return args.head
        }
        applyFunction(e, args)
      case _ => obj.Error(s"unknown node type '${node.tokenLiteral()}'")
    }

  private def evalExpressions(expressions: List[Expression])(implicit environment: Environment): List[obj.Object] = {
    val result: ListBuffer[obj.Object] = ListBuffer()
    expressions foreach { exp =>
      val e = eval(exp)
      if (isError(e)) {
        return List(e)
      }
      result += e
    }
    result.toList
  }

  private def applyFunction(fn: obj.Object, args: List[obj.Object]): obj.Object = {
    if (!fn.isInstanceOf[obj.Function]) {
      return obj.Error(s"not a function: ${fn.`type`()}")
    }
    val function = fn.asInstanceOf[obj.Function]
    val extendedEnv: Environment = extendFunctionEnv(function, args)
    val evaluated = eval(function.body)(extendedEnv)
    unwrapReturnValue(evaluated)
  }

  private def extendFunctionEnv(function: obj.Function, args: List[obj.Object]): Environment = {
    val env = Environment(outerEnv = Some(function.env))
    function.parameters.zipWithIndex.foreach { i =>
      if (i._2 < args.length) {
        env.set(i._1.value, args(i._2))
      }
    }
    env
  }

  private def unwrapReturnValue(value: obj.Object): obj.Object =
    value match {
      case r: Return => r.returnValue
      case _         => value
    }

  private def evalIdentifier(i: Identifier)(implicit environment: Environment): obj.Object =
    environment
      .get(i.value)
      .fold {
        obj.Error(s"identifier not found: ${i.value}").asInstanceOf[obj.Object]
      } { v =>
        v
      }

  private def isError(o: obj.Object): scala.Boolean =
    if (o != null) {
      o.`type`() == ERROR_OBJ
    } else {
      false
    }

  private def nativeBoolToBooleanObj(b: scala.Boolean): obj.Boolean = if (b) TRUE else FALSE

  private def evalInfixExpression(operator: String, left: obj.Object, right: obj.Object): obj.Object =
    (operator, left, right) match {
      case (_, _: obj.Integer, _: obj.Integer) => evalIntegerInfixExpression(operator, left, right)
      case (_, _: obj.Float, _: obj.Float)     => evalFloatInfixExpression(operator, left, right)
      case (_, _: obj.Integer, _: obj.Float) =>
        evalFloatInfixExpression(operator, obj.Float(left.asInstanceOf[obj.Integer].value.toFloat), right)
      case (_, _: obj.Float, _: obj.Integer) =>
        evalFloatInfixExpression(operator, left, obj.Float(right.asInstanceOf[obj.Integer].value.toFloat))
      //
      case ("==", _: obj.Float, _: obj.Float) => evalFloatInfixExpression(operator, left, right)
      //
      case ("==", _: obj.Integer, _: obj.Float) =>
        evalFloatInfixExpression(operator, obj.Float(left.asInstanceOf[obj.Integer].value.toFloat), right)
      case ("==", _: obj.Float, _: obj.Integer) =>
        evalFloatInfixExpression(operator, left, obj.Float(right.asInstanceOf[obj.Integer].value.toFloat))
      //
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

  private def evalFloatInfixExpression(operator: String, left: obj.Object, right: obj.Object): obj.Object = {
    val leftValue: scala.Float = left.asInstanceOf[obj.Float].value
    val rightValue: scala.Float = right.asInstanceOf[obj.Float].value
    operator match {
      case "+"  => obj.Float(leftValue + rightValue)
      case "-"  => obj.Float(leftValue - rightValue)
      case "*"  => obj.Float(leftValue * rightValue)
      case "/"  => obj.Float(leftValue / rightValue)
      case ">"  => nativeBoolToBooleanObj(leftValue.compareTo(rightValue) > 0)
      case "<"  => nativeBoolToBooleanObj(leftValue.compareTo(rightValue) < 0)
      case "==" => nativeBoolToBooleanObj(leftValue.compareTo(rightValue) == 0)
      case "!=" => nativeBoolToBooleanObj(leftValue.compareTo(rightValue) != 0)
      case "<=" => nativeBoolToBooleanObj(leftValue.compareTo(rightValue) <= 0)
      case ">=" => nativeBoolToBooleanObj(leftValue.compareTo(rightValue) >= 0)
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
    case obj.Float(f)                   => obj.Float(-1 * f)
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

  private def evalIfExpression(ife: IfExpression)(implicit env: Environment): obj.Object = {
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

  private def evalBlockStatement(blockStatement: BlockStatement)(implicit env: Environment): obj.Object = {
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

  private def evalProgram(statements: List[Statement])(implicit env: Environment): obj.Object = {
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
