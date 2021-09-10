// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

case class Program(statements: List[Statement] = List.empty) extends Node {

  override def toString: String = statements.map(_.toString).mkString("")

  override def tokenLiteral(): String =
    if (statements.nonEmpty) {
      statements.head.toString
    } else {
      ""
    }

}
