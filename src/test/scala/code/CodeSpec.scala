// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package code

import code.Opcodes._
import org.scalatest.flatspec.AnyFlatSpec

class CodeSpec extends AnyFlatSpec {

  "test make" should "pass it" in {
    List(
      (OpConstant, 65534, List[Byte](OpConstant, 255.toByte, 254.toByte))
    ) foreach { t =>
      val instruction = Code.make(t._1, t._2)
      assert(instruction.length == t._3.length)
      assert(instruction == t._3)
    }
  }

  "test instruction string" should "format as expected" in {
    val instructions = List[Instructions](
      Code.make(OpConstant, 1),
      Code.make(OpConstant, 2),
      Code.make(OpConstant, 65535)
    )
    val expectedStringOut =
      """
        |0000 OpConstant 1
        |0003 OpConstant 2
        |0006 OpConstant 65535
        |""".stripMargin

    val instructionOptStr = instructions.map(_.toString()).mkString("")
    assert(expectedStringOut == instructionOptStr)
  }

  "test read operands" should "pass" in {
    List(
      (OpConstant, List(65535), 2)
    ) foreach { tt =>
      val instruction = Code.make(tt._1, tt._2: _*)
      val de: Either[Throwable, Definition] = Code.lookup(tt._1)
      assert(de.isRight)
      val operandsRead: (List[Int], Int) = Code.ReadOperands(de.right.get, instruction.tail)
      assert(operandsRead._2 == tt._3)
      tt._2.zipWithIndex.foreach { t =>
        assert(operandsRead._1(t._2) != t._1)
      }
    }
  }

}
