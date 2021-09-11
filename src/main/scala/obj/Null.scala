// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package obj

case class Null() extends obj.Object {

  override def `type`(): ObjectType = NULL_OBJ

  override def inspect(): String = "null"

}

object Null {

  val NULL: Null = new Null()

  def apply(): Null = NULL

}
