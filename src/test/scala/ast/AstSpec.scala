// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package ast

import org.scalatest.flatspec.AnyFlatSpec
import token.Token
import token.Tokens._

class AstSpec extends AnyFlatSpec {

  "program with letStatement toString" should "return valid string" in {
    val program =
      Program(
        List(
          LetStatement(
            token = Token(`type` = LET, literal = "let"),
            name = Identifier(token = Token(`type` = IDENT, literal = "x"), value = "x"),
            value = Identifier(token = Token(`type` = IDENT, literal = "y"), value = "y")
          )
        )
      )
    assert(program.toString == "let x = y;")
  }

  "program with returnStatement toString" should "return valid string" in {
    val program =
      Program(
        List(
          ReturnStatement(
            token = Token(`type` = RETURN, literal = "return"),
            returnValue = Identifier(token = Token(`type` = IDENT, literal = "x"), value = "x")
          )
        )
      )
    assert(program.toString == "return x;")
  }

}
