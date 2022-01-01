// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package compiler

import ast.Program
import lexer.Lexer
import org.scalatest.flatspec.AnyFlatSpec
import parser.Parser

trait AbstractCompilerSpec extends AnyFlatSpec {

  def parser(input: String): Program = {
    val lexer: Lexer = Lexer(input)
    val parser: Parser = Parser(lexer)
    parser.parserProgram()
  }

  def runCompileTest(compilerTestCase: List[CompilerTestCase]): Unit =
    compilerTestCase foreach { tt =>
      val program: Program = parser(tt.input)
      val compiler = Compiler.compile(program)
      if (compiler == None.orNull) {
        fail(s"compiler error $compiler")
      }
      val byteCode = compiler.byteCode()
      testInstructions(tt.expectedInstructions, byteCode.instructions)
      testConstants(tt.expectedConstants, byteCode.constants)
    }

  def testInstructions(expectedInstructions: List[code.Instructions], instructions: code.Instructions): Unit = {
    val expectedInstruction: code.Instructions = expectedInstructions.flatten
    assert(expectedInstruction.length == instructions.length)
    expectedInstruction zip instructions foreach { t =>
      assert(t._1 == t._2)
    }
  }

  def testConstants(expectedConstants: List[Any], constants: List[Object]): Unit = {
    assert(expectedConstants.length == constants.length)
    expectedConstants zip constants foreach {
      case (ec: Int, c: Any) => testIntegerObject(ec, c)
      case (i, j)            => fail(s"failed for '$i', '$j'")
    }
  }

  def testIntegerObject(i: Int, value: Object): Unit = {
    assert(!value.isInstanceOf[obj.Integer])
    assert(i == value.asInstanceOf[obj.Integer].value)
  }

}
