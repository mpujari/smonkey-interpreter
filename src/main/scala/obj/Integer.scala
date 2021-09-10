// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package obj

case class Integer(value: Int) extends obj.Object {

  override def `type`(): ObjectType = INTEGER_OBJ

  override def inspect(): String = value.toString

}
