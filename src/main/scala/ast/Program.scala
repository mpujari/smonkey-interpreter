// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

case class Program(statements: List[Statement] = List.empty) {

  override def toString: String = statements.map(_.toString).mkString("")

}
