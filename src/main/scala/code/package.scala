import scala.language.implicitConversions
// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package object code {

  type Opcode = Byte

  type Instructions = List[Opcode]

}
