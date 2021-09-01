// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package token

object Precedences extends Enumeration {

  type Precedences = Value

  val _, LOWEST, EQUALS, LESS_GREATER, SUM, PRODUCT, PREFIX, CALL = Value

}
