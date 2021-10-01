// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package evaluator

import ast._
import obj.{ERROR_OBJ, FALSE, INTEGER_OBJ, NULL, Return, TRUE}
import obj._

import scala.collection.mutable
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
      case str: StringLiteral      => obj.SString(str.value)
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
      case a: ArrayLiteral =>
        val elements = evalExpressions(a.elements)
        if (elements.size == 1 && isError(elements.head)) {
          return elements.head
        }
        obj.Array(elements = evalExpressions(a.elements))
      case i: IndexExpression =>
        val left = eval(i.left)
        if (isError(left)) {
          return left
        }
        val index = eval(i.index)
        if (isError(index)) {
          return index
        }
        evalIndexExpression(left, index)
      case hashLiteral: HashLiteral => evalHashLiteral(hashLiteral)
      case _                        => obj.Error(s"unknown node type '${node.tokenLiteral()}'")
    }

  private def evalHashLiteral(hashLiteral: HashLiteral)(implicit environment: Environment): obj.Object = {
    val pairMap = mutable.HashMap[obj.Object, obj.Object]()
    hashLiteral.pair.map { hl =>
      val key = eval(hl._1)
      if (isError(key)) {
        return key
      }
      val value = eval(hl._2)
      if (isError(value)) {
        return value
      }
      pairMap += key -> value
    }
    Hash(pairs = pairMap.toMap)
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

  private def applyFunction(fn: obj.Object, args: List[obj.Object]): obj.Object =
    fn match {
      case f: obj.Function =>
        val extendedEnv: Environment = extendFunctionEnv(f, args)
        val evaluated = eval(f.body)(extendedEnv)
        unwrapReturnValue(evaluated)
      case b: obj.Builtin =>
        b.fn(args)
      case _ =>
        obj.Error(s"not a function: ${fn.`type`()}")
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

  private def evalIdentifier(i: Identifier)(implicit environment: Environment): obj.Object = {
    val value: Option[Object] = environment.get(i.value)
    if (value.nonEmpty) {
      value.get
    } else if (Builtins.builtins.contains(i.value)) {
      Builtins.builtins(i.value)
    } else {
      obj.Error(s"identifier not found: ${i.value}").asInstanceOf[obj.Object]
    }
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
      // format: off
      case (_, _: obj.SString, _: obj.SString)   => evalStringInfixExpression(operator, left, right)
      case (_, _: obj.SString, _)                => evalStringInfixExpression(operator, left, obj.SString(right.inspect()))
      case (_, _, _: obj.SString)                => evalStringInfixExpression(operator, obj.SString(left.inspect()), right)
      case (_, _: obj.Integer, _: obj.Integer)   => evalIntegerInfixExpression(operator, left, right)
      case (_, _: obj.Float, _: obj.Float)       => evalFloatInfixExpression(operator, left, right)
      case (_, _: obj.Integer, _: obj.Float)     => evalFloatInfixExpression(operator, obj.Float(left.asInstanceOf[obj.Integer].value.toFloat), right)
      case (_, _: obj.Float, _: obj.Integer)     => evalFloatInfixExpression(operator, left, obj.Float(right.asInstanceOf[obj.Integer].value.toFloat))
      case ("==", _, _)                          => nativeBoolToBooleanObj(left == right)
      case ("!=", _, _)                          => nativeBoolToBooleanObj(left != right)
      case (o, l, r) if l.`type`() != r.`type`() => obj.Error(s"type mismatch: ${l.`type`()} $o ${r.`type`()}")
      case (o, l, r)                             => obj.Error(s"unknown operator: ${l.`type`()} $o ${r.`type`()}")
      // format: on
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

  private def evalStringInfixExpression(operator: String, left: obj.Object, right: obj.Object): obj.Object = {
    val leftValue = left.asInstanceOf[obj.SString].value
    val rightValue = right.asInstanceOf[obj.SString].value
    operator match {
      case "+"  => obj.SString(leftValue + rightValue)
      case ">"  => nativeBoolToBooleanObj(leftValue > rightValue)
      case "<"  => nativeBoolToBooleanObj(leftValue < rightValue)
      case "==" => nativeBoolToBooleanObj(leftValue == rightValue)
      case "!=" => nativeBoolToBooleanObj(leftValue != rightValue)
      case "<=" => nativeBoolToBooleanObj(leftValue <= rightValue)
      case ">=" => nativeBoolToBooleanObj(leftValue >= rightValue)
      case _    => obj.Error(s"unknown operator: ${left.`type`()} $operator ${left.`type`()}")
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

  private def evalIndexExpression(left: Object, index: Object): obj.Object =
    (left.`type`(), index.`type`()) match {
      case (ARRAY_OBJ, INTEGER_OBJ) => evalArrayIndexExpression(left, index)
      case (HASH_OBJ, _)            => evalHashIndexExpression(left, index)
      case _                        => Error(s"index operator not supported: ${left.`type`()}")
    }

  private def evalHashIndexExpression(hashObj: Object, index: Object): obj.Object = {
    val hash = hashObj.asInstanceOf[obj.Hash]
    hash.pairs.getOrElse(index, NULL)
  }

  private def evalArrayIndexExpression(array: Object, index: Object): obj.Object = {
    val arrayObj = array.asInstanceOf[obj.Array]
    val i = index.asInstanceOf[obj.Integer].value
    val max = arrayObj.elements.size - 1
    if (i < 0 || i > max) {
      NULL
    } else {
      arrayObj.elements(i)
    }
  }

}
