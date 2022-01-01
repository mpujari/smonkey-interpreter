// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package compiler

case class CompilerTestCase(input: String, expectedConstants: List[Any], expectedInstructions: List[code.Instructions])
