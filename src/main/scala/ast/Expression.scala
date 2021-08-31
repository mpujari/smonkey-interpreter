// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

trait Expression extends Node {
  def expressionNode(): Unit
}
