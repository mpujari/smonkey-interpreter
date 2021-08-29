package lexer

import org.scalatest.FlatSpec
import token.Tokens._

class LexerSpec extends FlatSpec {

  "simple test with just idents" should "return expected tokens" in {
    Map(
      "abcd" -> List((IDENT, "abcd")),
      "ab cd" -> List((IDENT, "ab"), (IDENT, "cd")),
      "ab; cd" -> List((IDENT, "ab"), (SEMICOLON, ";"), (IDENT, "cd")),
      "abcd;" -> List((IDENT, "abcd"), (SEMICOLON, ";"))
    ) foreach { t =>
      val input = t._1
      val expectedTokens = t._2
      val lexer = Lexer(input)
      expectedTokens foreach { et =>
        val tok = lexer.nextToken()
        assert(tok.`type` == et._1)
        assert(tok.literal == et._2)
      }
    }
  }

  "simple op with no semicolon" should "return expected tokens" in {
    val input = "5 + 10"
    val expectedTokens = List(
      (INT, "5"),
      (PLUS, "+"),
      (INT, "10")
    )
    val lexer = Lexer(input)
    expectedTokens foreach { et =>
      val tok = lexer.nextToken()
      assert(tok.`type` == et._1)
      assert(tok.literal == et._2)
    }
  }

  "test nextToken with simple input" should "return expected tokens" in {
    val input = "=+(){},;+-%"
    val expectedTokens = List(
      (ASSIGN, "="),
      (PLUS, "+"),
      (LPAREN, "("),
      (RPAREN, ")"),
      (LBRACE, "{"),
      (RBRACE, "}"),
      (COMMA, ","),
      (SEMICOLON, ";"),
      (PLUS, "+"),
      (MINUS, "-"),
      (MOD, "%")
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
        |!-/*5%
        |5 < 10 > 5
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
      (SEMICOLON, ";"),
      (BANG, "!"),
      (MINUS, "-"),
      (SLASH, "/"),
      (ASTERISK, "*"),
      (INT, "5"),
      (MOD, "%"),
      (INT, "5"),
      (LT, "<"),
      (INT, "10"),
      (GT, ">"),
      (INT, "5")
    )
    val lexer = Lexer(input)
    expectedTokens foreach { et =>
      val tok = lexer.nextToken()
      assert(tok.`type` == et._1)
      assert(tok.literal == et._2)
    }
  }

  "test nextToken with keywords return, true, false if" should "return expected tokens" in {
    val input =
      """
        |if (5 < 10) {
        |   return true;
        |} else {
        |   return false;
        |}
        |""".stripMargin
    val expectedTokens = List(
      (IF, "if"),
      (LPAREN, "("),
      (INT, "5"),
      (LT, "<"),
      (INT, "10"),
      (RPAREN, ")"),
      (LBRACE, "{"),
      (RETURN, "return"),
      (TRUE, "true"),
      (SEMICOLON, ";"),
      (RBRACE, "}"),
      (ELSE, "else"),
      (LBRACE, "{"),
      (RETURN, "return"),
      (FALSE, "false"),
      (SEMICOLON, ";"),
      (RBRACE, "}")
    )
    val lexer = Lexer(input)
    expectedTokens foreach { et =>
      val tok = lexer.nextToken()
      assert(tok.`type` == et._1)
      assert(tok.literal == et._2)
    }
  }

  "test nextToken with two-character token" should "identify and return expected tokens" in {
    val input =
      """
        | 10 == 10
        | 10 != 9
        |""".stripMargin
    val expectedTokens = List(
      (INT, "10"),
      (EQ, "=="),
      (INT, "10"),
      (INT, "10"),
      (NOT_EQ, "!="),
      (INT, "9")
    )
    val lexer = Lexer(input)
    expectedTokens foreach { et =>
      val tok = lexer.nextToken()
      assert(tok.`type` == et._1)
      assert(tok.literal == et._2)
    }
  }

}
