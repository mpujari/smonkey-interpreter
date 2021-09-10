// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

import ast.Program
import evaluator.Evaluator
import lexer.Lexer
import parser.Parser

import java.util.Scanner
import scala.io.Source

object Main {

  lazy val monkeyHappyFace: String = {
    val x = Source.fromResource("monkey_happy_face.txt")
    val face = x.mkString
    x.close()
    face
  }

  lazy val monkeySadFace: String = {
    val x = Source.fromResource("monkey_sad_face.txt")
    val face = x.mkString
    x.close()
    face
  }

  def main(args: Array[String]): Unit = {
    val user = if (System.getProperty("user.name") != null) {
      System.getProperty("user.name")
    } else {
      ""
    }
    println(s"Hello $user! This is the Monkey programming language!")
    println("Feel free to type in commands")
    start()
  }

  def start(): Unit = {
    val scanner = new Scanner(System.in)
    while (true) {
      print(">> ")
      val line = scanner.nextLine()
      if (checkForExit(line)) {
        println(monkeyHappyFace)
        println("Bye!!!")
        System.exit(0)
      }
      val lexer = Lexer(line)

      val p: Parser = Parser(lexer)
      val program: Program = p.parserProgram()

      if (p.getErrors.nonEmpty) {
        printParseError(p.getErrors)
      } else {
        val eval = Evaluator.eval(program)
        if (eval != null) {
          println(eval.inspect())
        }
      }
    }
  }

  def printParseError(errors: List[String]): Unit = {
    println(monkeySadFace)
    println("Woops! We ran into some monkey business here!")
    println(" parse error:")
    errors foreach { e =>
      println(s"\t$e")
    }
  }

  private val exitCmds = List(
    "exit",
    "quit",
    "bye"
  )

  private def checkForExit(str: String): Boolean = exitCmds.contains(str.toLowerCase())

}
