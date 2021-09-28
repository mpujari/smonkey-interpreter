// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package evaluator

object Builtins {

  val builtins: Map[String, obj.Object] =
    Map[String, obj.Builtin](
      "len" -> obj.Builtin(lenBuiltin),
      "print" -> obj.Builtin(printBuiltin),
      "first" -> obj.Builtin(firstBuiltin),
      "last" -> obj.Builtin(lastBuiltin),
      "rest" -> obj.Builtin(restBuiltin),
      "push" -> obj.Builtin(pushBuiltin)
    )

  private def printBuiltin: Seq[obj.Object] => obj.Object = (args: Seq[obj.Object]) => {
    if (args.length != 1) {
      obj.Error(s"wrong number of arguments to 'print', got=${args.length}, want=1")
    } else {
      obj.SString(value = args.head.inspect())
    }
  }

  private def lenBuiltin: Seq[obj.Object] => obj.Object = (args: Seq[obj.Object]) => {
    if (args.length != 1) {
      obj.Error(s"wrong number of arguments to 'len', got=${args.length}, want=1")
    } else {
      args.head match {
        case s: obj.SString => obj.Integer(value = s.value.length)
        case a: obj.Array   => obj.Integer(value = a.elements.size)
        case e @ _          => obj.Error(s"arguments to 'len' not supported, got ${e.`type`()}")
      }
    }
  }

  private def firstBuiltin: Seq[obj.Object] => obj.Object = (args: Seq[obj.Object]) => {
    if (args.length != 1) {
      obj.Error(s"wrong number of arguments to 'first', got=${args.length}, want=1")
    } else {
      args.head match {
        case a: obj.Array =>
          if (a.elements.nonEmpty) {
            a.elements.head
          } else {
            obj.NULL
          }
        case e @ _ => obj.Error(s"arguments to 'first' not supported, got ${e.`type`()}")
      }
    }
  }

  private def lastBuiltin: Seq[obj.Object] => obj.Object = (args: Seq[obj.Object]) => {
    if (args.length != 1) {
      obj.Error(s"wrong number of arguments to 'last', got=${args.length}, want=1")
    } else {
      args.head match {
        case a: obj.Array =>
          if (a.elements.nonEmpty) {
            a.elements.last
          } else {
            obj.NULL
          }
        case e @ _ => obj.Error(s"arguments to 'last' not supported, got ${e.`type`()}")
      }
    }
  }

  private def restBuiltin: Seq[obj.Object] => obj.Object = (args: Seq[obj.Object]) => {
    if (args.length != 1) {
      obj.Error(s"wrong number of arguments to 'rest', got=${args.length}, want=1")
    } else {
      args.head match {
        case a: obj.Array =>
          if (a.elements.nonEmpty) {
            obj.Array(elements = a.elements.tail)
          } else {
            obj.NULL
          }
        case e @ _ => obj.Error(s"arguments to 'rest' not supported, got ${e.`type`()}")
      }
    }
  }

  private def pushBuiltin: Seq[obj.Object] => obj.Object = (args: Seq[obj.Object]) => {
    if (args.length != 2) {
      obj.Error(s"wrong number of arguments to 'push', got=${args.length}, want=2")
    } else {
      args.head match {
        case a: obj.Array => obj.Array(elements = a.elements :+ args(1))
        case e @ _        => obj.Error(s"arguments to 'push' not supported, got ${e.`type`()}")
      }
    }
  }

}
