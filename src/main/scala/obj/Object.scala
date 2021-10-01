// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package obj

trait Object {

  type ObjectType = String

  def `type`(): ObjectType

  def inspect(): String

}
