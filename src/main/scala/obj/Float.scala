// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package obj

case class Float(value: scala.Float) extends obj.Object {

  override def `type`(): ObjectType = FLOAT_OBJ

  override def inspect(): String = value.toString

}
