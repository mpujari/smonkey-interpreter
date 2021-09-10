// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package evaluator

import ast._

import scala.annotation.tailrec

object Evaluator {

  @tailrec
  def eval(node: Node): obj.Object =
    node match {
      case p: Program              => evalStatements(p.statements)
      case es: ExpressionStatement => eval(es.expression)
      case i: IntegerLiteral       => obj.Integer(i.value)
      case b: BooleanLiteral       => obj.Boolean(b.value)
      case _                       => null
    }

  private def evalStatements(statements: List[Statement]): obj.Object = {
    var result: obj.Object = obj.Null()
    statements.foreach { s: Statement =>
      result = eval(s)
    }
    result
  }

}
