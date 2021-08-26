import lexer.Lexer
import token.{Token, Tokens}

import java.util.Scanner
// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

object Main {

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
      val lexer = Lexer(line)
      var tok: Token = lexer.nextToken()
      while (tok.`type` != Tokens.EOF) {
        println(tok)
        tok = lexer.nextToken()
      }
    }
  }
}
