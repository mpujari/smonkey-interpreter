// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package evaluator

object Builtins {

  val builtins: Map[String, obj.Object] =
    Map[String, obj.Builtin]("len" -> obj.Builtin(lenBuiltin), "print" -> obj.Builtin(printBuiltin))

  private def lenBuiltin: Seq[obj.Object] => obj.Object = (args: Seq[obj.Object]) => {
    if (args.length != 1) {
      obj.Error(s"wrong number of arguments, got=${args.length}, want=1")
    } else {
      args.head match {
        case s: obj.SString => obj.Integer(value = s.value.length)
        case e @ _          => obj.Error(s"arguments to 'len' not supported, got ${e.`type`()}")
      }
    }
  }

  private def printBuiltin: Seq[obj.Object] => obj.Object = (args: Seq[obj.Object]) => {
    if (args.length != 1) {
      obj.Error(s"wrong number of arguments, got=${args.length}, want=1")
    } else {
      obj.SString(value = args.head.inspect())
    }
  }

}
