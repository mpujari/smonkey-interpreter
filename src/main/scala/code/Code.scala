// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package code

import code.Opcodes._

import java.nio.ByteBuffer
import scala.collection.mutable.ListBuffer

object Code {

  val definitions: Map[code.Opcode, Definition] =
    Map(OpConstant -> Definition("OpConstant", List(2)), OpAdd -> Definition("OpAdd"))

  def make(op: Opcode, operands: Int*): List[Byte] =
    lookup(op) match {
      case Left(_) => List[Byte]()
      case Right(d) =>
        var instructionLen = 1
        d.operandWidths foreach { i =>
          instructionLen = instructionLen + i
        }
        val instruction = ListBuffer[Byte](op)
        var offset = 1
        operands.zipWithIndex foreach { o =>
          val width = d.operandWidths(o._2)
          width match {
            case 2 =>
              val bb: ByteBuffer = ByteBuffer.allocate(2 * 2)
              bb.putInt(o._1)
              instruction ++= bb.array().slice(2, 4)
            case _ => // TODO no-ops or logging
          }
          offset = offset + width
        }
        instruction.toList
    }

  // TODO revisit me wrt map and code as key
  def lookup(op: Byte): Either[Throwable, Definition] = {
    val opCode = Opcodes.getOpCode(op)
    if (opCode.isDefined) {
      Right(definitions(opCode.get))
    } else {
      Left(new Throwable(s"opcode $op undefined"))
    }
  }

  def ReadOperands(definition: Definition, instructions: Instructions): (List[Int], Int) = {
    val listBuffer = ListBuffer[Int]()
    var offset = 0
    definition.operandWidths.zipWithIndex foreach { d =>
      d._1 match {
        case 2 =>
          listBuffer ++= instructions.slice(offset, instructions.size).map(_.toInt)
      }
      offset = offset + d._1
    }
    (listBuffer.toList, offset)
  }

}
