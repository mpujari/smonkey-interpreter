// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package token

case class Token(`type`: String, literal: String) {

  override def toString: String = {
    s"{Type:${`type`} Literal:$literal}"
  }

}
