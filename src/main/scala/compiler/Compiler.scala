// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package compiler

case class Compiler(instructions: code.Instructions, constants: List[obj.Object]) {

  def byteCode(): ByteCode = ByteCode(instructions = instructions, constants = constants)

}

object Compiler {

  def compile(node: ast.Node): Compiler = None.orNull

}
