// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package obj

case class Array(elements: List[obj.Object]) extends obj.Object {

  override def `type`(): ObjectType = ARRAY_OBJ

  override def inspect(): String =
    s"[${elements.map(_.inspect()).mkString(", ")}]"

}
