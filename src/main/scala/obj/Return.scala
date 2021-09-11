// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package obj

case class Return(returnValue: obj.Object) extends obj.Object {

  override def `type`(): ObjectType = RETURN_OBJ

  override def inspect(): String = returnValue.inspect()

}
