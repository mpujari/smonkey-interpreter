// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package obj

case class Error(errorMsg: String) extends obj.Object {

  override def `type`(): ObjectType = ERROR_OBJ

  override def inspect(): String = s"ERROR: $errorMsg"

}
