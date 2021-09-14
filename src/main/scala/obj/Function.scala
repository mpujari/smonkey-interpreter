// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package obj

import ast.{BlockStatement, Identifier}

case class Function(parameters: List[Identifier], body: BlockStatement, env: Environment) extends obj.Object {

  override def `type`(): ObjectType = FUNCTION_OBJ

  override def inspect(): String = s"fn(${parameters.map(_.toString).mkString(", ")}) {\n ${body.toString} \n}"

}
