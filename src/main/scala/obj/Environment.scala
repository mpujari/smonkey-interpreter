// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package obj

import scala.collection.mutable

case class Environment(
    store: mutable.Map[String, obj.Object] = mutable.Map[String, obj.Object](),
    outerEnv: Option[Environment] = None) {

  def get(name: String): Option[obj.Object] = {
    val v1 = store.get(name)
    if (v1.isEmpty && outerEnv.nonEmpty) {
      outerEnv.get.get(name)
    } else {
      v1
    }
  }

  def set(name: String, o: obj.Object): Unit = store(name) = o

}
