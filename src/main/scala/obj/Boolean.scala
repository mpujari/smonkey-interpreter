// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package obj

case class Boolean(value: scala.Boolean) extends obj.Object {

  override def `type`(): ObjectType = BOOLEAN_OBJ

  override def inspect(): String = value.toString

}

object Boolean {

  val TRUE: obj.Boolean = new obj.Boolean(true)

  val FALSE: obj.Boolean = new obj.Boolean(false)

  def apply(value: scala.Boolean): obj.Boolean =
    if (value) {
      TRUE
    } else {
      FALSE
    }

}
