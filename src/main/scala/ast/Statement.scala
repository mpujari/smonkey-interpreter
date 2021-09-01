// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

trait Statement extends Node {

  def statementNode(): Unit

  override def toString: String = ???

}
