// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package compiler

import org.scalatest.flatspec.AnyFlatSpec
import code.Code._

class CompilerSpec extends AnyFlatSpec with AbstractCompilerSpec {

  "test integer arithmetic" should "pass the test" in {
    val tests = List[CompilerTestCase] {
      CompilerTestCase(input = "1 + 2",
                       expectedConstants = List[Any](1, 2),
                       expectedInstructions = List(
                         make(
                           code.Opcodes.OpConstant,
                           0
                         )
                       ))
    }
    runCompileTest(tests)
  }

}
