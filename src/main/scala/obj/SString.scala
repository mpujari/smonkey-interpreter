// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package obj

case class SString(value: String) extends obj.Object {

  override def `type`(): ObjectType = STRING_OBJ

  override def inspect(): String = value

}
