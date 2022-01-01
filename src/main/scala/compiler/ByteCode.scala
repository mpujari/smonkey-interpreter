// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package compiler

case class ByteCode(instructions: code.Instructions, constants: List[obj.Object])
