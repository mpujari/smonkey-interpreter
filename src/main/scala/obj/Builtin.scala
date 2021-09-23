// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package obj

case class Builtin(fn: Seq[obj.Object] => obj.Object) extends obj.Object {

  override def `type`(): ObjectType = BUILTIN_OBJ

  override def inspect(): String = "builtin function"

}
