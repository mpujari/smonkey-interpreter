package lexer

import org.scalatest.FlatSpec
import token.Tokens._

class LexerSpec extends FlatSpec {

  "test nextToken with simple input" should "return expected tokens" in {
    val input = "=+(){},;"
    val expectedTokens = List(
      (ASSIGN, "="),
      (PLUS, "+"),
      (LPAREN, "("),
      (RPAREN, ")"),
      (LBRACE, "{"),
      (RBRACE, "}"),
      (COMMA, ","),
      (SEMICOLON, ";")
    )
    val lexer = Lexer(input)
    expectedTokens foreach { et =>
      val tok = lexer.nextToken()
      assert(tok.`type` == et._1)
      assert(tok.literal == et._2)
    }
  }

  "test nextToken with simple input and let ident" should "return expected tokens" in {
    val input =
      """
        |let five = 5;
        |let ten = 10;
        |
        |let add = fn(x, y) {
        |  x + y;
        |}
        |
        |let result = add(five, ten);
        |""".stripMargin
    val expectedTokens = List(
      (LET, "let"),
      (IDENT, "five"),
      (ASSIGN, "="),
      (INT, "5"),
      (SEMICOLON, ";"),
      (LET, "let"),
      (IDENT, "ten"),
      (ASSIGN, "="),
      (INT, "10"),
      (SEMICOLON, ";"),
      (LET, "let"),
      (IDENT, "add"),
      (ASSIGN, "="),
      (FUNCTION, "fn"),
      (LPAREN, "("),
      (IDENT, "x"),
      (COMMA, ","),
      (IDENT, "y"),
      (RPAREN, ")"),
      (LBRACE, "{"),
      (IDENT, "x"),
      (PLUS, "+"),
      (IDENT, "y"),
      (SEMICOLON, ";"),
      (RBRACE, "}"),
      (LET, "let"),
      (IDENT, "result"),
      (ASSIGN, "="),
      (IDENT, "add"),
      (LPAREN, "("),
      (IDENT, "five"),
      (COMMA, ","),
      (IDENT, "ten"),
      (RPAREN, ")"),
      (SEMICOLON, ";")
    )
    val lexer = Lexer(input)
    expectedTokens foreach { et =>
      val tok = lexer.nextToken()
      assert(tok.`type` == et._1)
      assert(tok.literal == et._2)
    }
  }

}
