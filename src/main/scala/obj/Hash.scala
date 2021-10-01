// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package obj

case class Hash(pairs: Map[obj.Object, obj.Object]) extends obj.Object {

  override def `type`(): ObjectType = HASH_OBJ

  override def inspect(): String =
    s"{${pairs.map(e => (e._1.inspect(), e._2.inspect())).mkString(", ")}}".replaceAll(" ->", ":")

}
