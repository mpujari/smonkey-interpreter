// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package code

import java.util.concurrent.atomic.AtomicInteger
import scala.language.implicitConversions

object Opcodes {

  private val i: AtomicInteger = new AtomicInteger(0)

  private implicit def toByte(i: Int): Byte = i.toByte

  val OpConstant: Opcode = i.incrementAndGet()

  val OpAdd: Opcode = i.incrementAndGet()

  private val values: Set[Opcode] = Set(OpConstant, OpAdd)

  def getOpCode(op: Opcode): Option[Opcode] = values.find(p => p == op)

}
