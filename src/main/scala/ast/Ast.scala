// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

class Ast {

  def tokenLiteral(program: Program): String =
    if (program.statements.nonEmpty) {
      program.statements.head.tokenLiteral()
    } else {
      ""
    }

}
