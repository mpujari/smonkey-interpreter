// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package evaluator

import aabstract.AbstractBaseSpec
import obj.NULL
import org.scalatest.FlatSpec

class EvaluatorSpec extends FlatSpec with AbstractBaseSpec {

  "test eval integer" should "pass the tests" in {
    List(("5", 5), ("10", 10), ("-5", -5), ("-10", -10)) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      testIntegerObject(evaluated, t._2)
    }
  }

  "test eval boolean" should "pass the tests" in {
    List(
      ("true", true),
      ("false", false),
      ("!-5", false),
      ("!!-5", true),
      ("!-0.5", false),
      ("!!-0.5", true),
      ("true", true),
      ("false", false),
      ("1 < 2", true),
      ("1 > 2", false),
      ("1 < 1", false),
      ("1 > 1", false),
      ("1 == 1", true),
      ("1 != 1", false),
      ("1 == 2", false),
      ("1 != 2", true),
      ("1 >= 1", true),
      ("1 <= 1", true),
      ("1 >= 5", false),
      ("5 <= 1", false),
      ("0.1 < 0.2", true),
      ("0.1 > 0.2", false),
      ("0.1 < 0.1", false),
      ("0.1 > 0.1", false),
      ("0.1 == 0.1", true),
      ("0.1 != 0.1", false),
      ("0.1 == 0.2", false),
      ("0.1 != 0.2", true),
      ("0.1 >= 0.1", true),
      ("0.1 <= 0.1", true),
      ("0.1 >= 0.5", false),
      ("0.5 <= 0.1", false),
      ("-0.1 < -0.2", false),
      ("-0.1 > -0.2", true),
      ("-0.1 < -0.1", false),
      ("-0.1 > -0.1", false),
      ("-0.1 == -0.1", true),
      ("-0.1 != -0.1", false),
      ("-0.1 == -0.2", false),
      ("-0.1 != -0.2", true),
      ("-0.1 >= -0.1", true),
      ("-0.1 <= -0.1", true),
      ("-0.1 >= -0.5", true),
      ("-0.5 <= -0.1", true),
      ("0.1 < 2", true),
      ("0.1 > 2", false),
      ("0.1 < 1", true),
      ("0.1 > 1", false),
      ("0.1 == 1", false),
      ("0.1 != 1", true),
      ("0.1 == 2", false),
      ("0.1 != 2", true),
      ("0.1 >= 1", false),
      ("0.1 <= 1", true),
      ("0.1 >= 5", false),
      ("0.1 <= 1", true),
      ("1 < 0.2", false),
      ("1 > 0.2", true),
      ("1 < 0.1", false),
      ("1 > 0.1", true),
      ("1 == 0.1", false),
      ("1 != 0.1", true),
      ("1 == 0.2", false),
      ("1 != 0.2", true),
      ("1 >= 0.1", true),
      ("1 <= 0.1", false),
      ("1 >= 0.5", true),
      ("1 <= 0.1", false),
      ("!(1 >= 1)", false),
      ("!(1 <= 1)", false),
      ("!(1 >= 5)", true),
      ("!(5 <= 1)", true),
      ("true == true", true),
      ("false == false", true),
      ("true == false", false),
      ("true != false", true),
      ("false != true", true),
      ("(1 < 2) == true", true),
      ("(1 < 2) == false", false),
      ("(1 > 2) == true", false),
      ("(1 > 2) == false", true)
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      testBooleanObject(evaluated, t._2, Some(s"Test failed for '${t._1}'"))
    }
  }

  "test eval bang prefix operator" should "pass the tests" in {
    List(
      ("!true", false),
      ("!false", true),
      ("!5", false),
      ("!!true", true),
      ("!!false", false),
      ("!!!true", false),
      ("!!!false", true),
      ("!!5", true),
      ("(5 > 5 == true) != false", false),
      ("(0.5 > 0.5 == true) != false", false),
      ("(10 + 2) * 30 == 300 + 20 * 3", true),
      ("(10.0 + 2.0) * 30.0 == 300.0 + 20.0 * 3.0", true),
      ("(10.01 + 2.01) * 30.01 == 300.01 + 20.01 * 3.01 + 0.4801", true),
      ("(5 > 5 == true) != false", false),
      ("(5.01 > 5.01 == true) != false", false),
      ("(-5.01 > -5.01 == true) != false", false),
      ("500 / 2 != 250", false),
      ("500 / 2 == 250", true),
      ("500.0 / 2.0 != 250.0", false),
      ("500.01 / 2.01 == 248.7612", true),
      ("5 * 10 > 40 + 5", true),
      ("5 * 10 < 40 + 5", false),
      ("5.0 * 10.0 > 40.0 + 5.0", true),
      ("5.0 * 10.0 < 40.0 + 5.0", false),
      ("5 * 10.0 > 40 + 5.0", true),
      ("5.0 * 10 < 40.0 + 5", false),
      ("3 + 4 * 5 == 3 * 1 + 4 * 5", true),
      ("3 + 4 * 5 != 3 * 1 + 4 * 5", false),
      ("3 + 4 * 5 <= 3 * 1 + 4 * 5", true),
      ("3 + 4 * 5 >= 3 * 1 + 4 * 5", true),
      ("3 + 4 * 5 < 3 * 1 + 4 * 5", false),
      ("3 + 4 * 5 > 3 * 1 + 4 * 5", false),
      ("3 + 4 * 5 > 3 * 1 + 4 * 5", false),
      ("3 + 4 * 5 < 3 * 1 + 4 * 5", false),
      ("3.0 + 4.0 * 5.0 == 3.0 * 1.0 + 4.0 * 5.0", true),
      ("3.0 + 4.0 * 5.0 != 3.0 * 1.0 + 4.0 * 5.0", false),
      ("3.0 + 4.0 * 5.0 <= 3.0 * 1.0 + 4.0 * 5.0", true),
      ("3.0 + 4.0 * 5.0 >= 3.0 * 1.0 + 4.0 * 5.0", true),
      ("3.0 + 4.0 * 5.0 < 3.0 * 1.0 + 4.0 * 5.0", false),
      ("3.0 + 4.0 * 5.0 > 3.0 * 1.0 + 4.0 * 5.0", false),
      ("3.0 + 4.0 * 5.0 > 3.0 * 1.0 + 4.0 * 5.0", false),
      ("3.0 + 4.0 * 5.0 < 3.0 * 1.0 + 4.0 * 5.0", false),
      ("3.1 + 4.1 * 5.1 == 3.1 * 1.1 + 4.1 * 5.1 - 0.31", true),
      ("3.1 + 4.1 * 5.1 != 3.1 * 1.1 + 4.1 * 5.1 - 0.31", false),
      ("3.1 + 4.1 * 5.1 <= 3.1 * 1.1 + 4.1 * 5.1 - 0.31", true),
      ("3.1 + 4.1 * 5.1 >= 3.1 * 1.1 + 4.1 * 5.1 - 0.31", true),
      ("3.1 + 4.1 * 5.1 < 3.1 * 1.1 + 4.1 * 5.1 - 0.31", false),
      ("3.1 + 4.1 * 5.1 > 3.1 * 1.1 + 4.1 * 5.1 - 0.31", false),
      ("3.1 + 4.1 * 5.1 > 3.1 * 1.1 + 4.1 * 5.1 - 0.31", false),
      ("3.1 + 4.1 * 5.1 < 3.1 * 1.1 + 4.1 * 5.1 - 0.31", false)
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      testBooleanObject(evaluated, t._2, Some(s"Failed for '${t._1}'"))
    }
  }

  "test eval infix operator" should "pass the tests" in {
    List(
      ("5 + 5", 10),
      ("0 + 0", 0),
      ("5 - 5", 0),
      ("-5 - 5", -10),
      ("1 * 0", 0),
      ("-1 * -1", 1),
      ("-1 * -10", 10),
      ("1 * -10", -10),
      ("5", 5),
      ("10", 10),
      ("-5", -5),
      ("-10", -10),
      ("5 + 5 + 5 + 5 - 10", 10),
      ("2 * 2 * 2 * 2 * 2", 32),
      ("-50 + 100 + -50", 0),
      ("5 * 2 + 10", 20),
      ("5 + 2 * 10", 25),
      ("20 + 2 * -10", 0),
      ("50 / 2 * 2 + 10", 60),
      ("2 * (5 + 10)", 30),
      ("3 * 3 * 3 + 10", 37),
      ("3 * (3 * 3) + 10", 37),
      ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      testIntegerObject(evaluated, t._2)
    }
  }

  "test eval infix operator with float" should "pass the tests" in {
    List(
      ("5.01 + 5.001", 10.011f),
      ("0.0 + 0.0", 0f),
      ("5.01 - 5.01", 0f),
      ("-5.01 - 5.01", -10.02f),
      ("1.01 * 0.0", 0.0f),
      ("-1.01 * -1.01", 1.0201f),
      ("-1.01 * -10.01", 10.1101f),
      ("1.01 * -10.01", -10.1101f),
      ("5.0", 5f),
      ("10.0", 10f),
      ("-5.0", -5f),
      ("-10.0", -10f),
      ("5.01 + 5.01 + 5.01 + 5.01 - 10.01", 10.030001f), // TODO revisit me, for precision
      ("2.0 * 2.0 * 2.0 * 2.0 * 2.01", 32.16f),
      ("2.0 * -2.0 * 2.0 * 2.0 * -2.01", 32.16f),
      ("-2.0 * -2.0 * 2.0 * 2.0 * -2.01", -32.16f),
      ("-50.01 + 100.01 + -50.01", -0.009994507f), // TODO revisit me, for precision
      ("5.01 * 2.01 + 10.01", 20.080101f),
      ("5.0 + 2.0 * 10.0", 25.0f),
      ("20.0 + 2.0 * -10.0", 0f),
      ("50.0 / 2.0 * 2.0 + 10.0", 60.0f),
      ("2.0 * (5.0 + 10.0)", 30.0f),
      ("3.0 * 3.0 * 3.0 + 10.0", 37.0f),
      ("3.0 * (3.0 * 3.0) + 10.0", 37.0f),
      ("(5.0 + 10.0 * 2.0 + 15.0 / 3.0) * 2.0 + -10.0", 50.0f)
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      testFloatObject(evaluated, t._2, Some(s"Failed for ${t._1}"))
    }
  }

  "test if-else expression" should "pass the tests" in {
    List(
      ("if (true) { 10 }", 10),
      ("if (false) { 10 }", NULL),
      ("if (1) { 10 }", 10),
      ("if (1 < 2) { 10 }", 10),
      ("if (1 > 2) { 10 }", NULL),
      ("if (1 > 2) { 10 } else { 20 }", 20),
      ("if (1 < 2) { 10 } else { 20 }", 10),
      ("if (5 * 5 + 10 > 34) { 99 } else { 100 }", 99),
      ("if ((1000 / 2) + 250 * 2 == 1000) { 9999 }", 9999)
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      if (t._2 == NULL) {
        testNullObject(evaluated)
      } else {
        testIntegerObject(evaluated, t._2.toString.toInt, Some(s"Failed for '${t._1}'"))
      }
    }
  }

  "test if-else expression with float" should "pass the tests" in {
    List(
      ("if (true) { 10.01 }", 10.01f),
      ("if (false) { 10.01 }", NULL),
      ("if (1.01) { 10.01 }", 10.01f),
      ("if (-1.01) { -10.01 }", -10.01f),
      ("if (1.01 < 2.01) { 10.01 }", 10.01f),
      ("if (1 < 2.01) { 10.01 }", 10.01f),
      ("if (1.01 < 2) { 10.01 }", 10.01f),
      ("if (1.01 > 2.01) { 10.01 }", NULL),
      ("if (1 > 2.01) { 10.01 }", NULL),
      ("if (1.01 > 2) { 10.01 }", NULL),
      ("if (1.01 > 2.01) { 10.01 } else { 20.01 }", 20.01f),
      ("if (1 > 2.01) { 10.01 } else { 20.01 }", 20.01f),
      ("if (1.01 > 2) { 10.01 } else { 20.01 }", 20.01f),
      ("if (1.01 < 2.01) { 10.01 } else { 20.01 }", 10.01f),
      ("if (1 < 2.01) { 10.01 } else { 20.01 }", 10.01f),
      ("if (1.01 < 2) { 10.01 } else { 20.01 }", 10.01f),
      ("if (5.01 * 5.01 + 10.01 > 34.01) { 99.01 } else { 100.01 }", 99.01f),
      ("if ((1000.01 / 2.01) + 250.01 * 2.01 == 1000.0375) { 9999.01 }", 9999.01f) // TODO revisit me, for precision
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      if (t._2 == NULL) {
        testNullObject(evaluated)
      } else {
        testFloatObject(evaluated, t._2.toString.toFloat, Some(s"Failed for '${t._1}'"))
      }
    }
  }

  "test return statements" should "pass the tests" in {
    List(
      ("return 10;", 10),
      ("return 10; 9;", 10),
      ("return 2 * 5; 9;", 10),
      ("9; return 2 * 5; 9;", 10),
      ("if (10 > 1) {if (10 > 1) {return 10;}return 1;}", 10)
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      testIntegerObject(evaluated, t._2, Some(s"Failed for '${t._1}'"))
    }
  }

  "test return statements with float" should "pass the tests" in {
    List(
      ("return 10.01;", 10.01f),
      ("return -10.01;", -10.01f),
      ("return 10.01; 9;", 10.01f),
      ("return 2.0 * 5.0; 9;", 10.0f),
      ("9; return 2.0 * 5.0; 9;", 10.0f),
      ("if (10 > 1) {if (10 > 1) {return 10.01;}return 1;}", 10.01f)
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      testFloatObject(evaluated, t._2, Some(s"Failed for '${t._1}'"))
    }
  }

  "Error Handling test" should "pass the tests" in {
    List(
      ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
      ("5.0 + true;", "type mismatch: FLOAT + BOOLEAN"),
      ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
      ("5.01 + true; 5.01;", "type mismatch: FLOAT + BOOLEAN"),
      ("-true", "unknown operator: -BOOLEAN"),
      ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
      ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
      ("5.01; true + false; 5.01", "unknown operator: BOOLEAN + BOOLEAN"),
      ("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN"),
      ("if (10.0 > 1.01) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN"),
      ("foobar", "identifier not found: foobar"),
      (
        """
          if (10 > 1) {
            if (10 > 1) {
              return true + false;
            }
              return 1;
            }
          }
        """,
        "unknown operator: BOOLEAN + BOOLEAN",
      )
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      assert(evaluated.isInstanceOf[obj.Error])
      val error = evaluated.asInstanceOf[obj.Error]
      assert(error.errorMsg == t._2, s"Failed for '${t._1}'")
    }
  }

  "let statement with env" should "pass the tests" in {
    List(
      ("let a = 5; a;", 5),
      ("let a = 5 * 5; a;", 25),
      ("let a = 5; let b = a; b;", 5),
      ("let a = 5; let b = a; let c = a + b + 5; c;", 15)
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      testIntegerObject(evaluated, expected = t._2)
    }
  }

  "let statement with env and with float" should "pass the tests" in {
    List(
      ("let a = 5.01; a;", 5.01f),
      ("let a = 5.01 * 5.01; a;", 25.100101f),
      ("let a = 5.01; let b = a; b;", 5.01f),
      ("let a = 5.01; let b = a; let c = a + b + 5.01; c;", 15.030001f)
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      testFloatObject(evaluated, expected = t._2)
    }
  }

  "test function param, body" should "pass the tests" in {
    List(
      "fn(x) { x + 2; };",
      "fn(x) { x + 2.01; };"
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t)
      assert(evaluated.isInstanceOf[obj.Function])
      val fn: obj.Function = evaluated.asInstanceOf[obj.Function]
      assert(fn.parameters.size == 1)
      assert(fn.parameters.head.toString == "x")
      if (t.contains("2.01")) {
        assert(fn.body.toString == "(x + 2.01)")
      } else {
        assert(fn.body.toString == "(x + 2)")
      }
    }
  }

  "test function calls" should "pass the tests" in {
    List(
      ("let identity = fn(x) { x; }; identity(5);", 5),
      ("let identity = fn(x) { return x; }; identity(5);", 5),
      ("let double = fn(x) { x * 2; }; double(5);", 10),
      ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
      ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
      ("fn(x) { x; }(5)", 5)
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      testIntegerObject(evaluated, t._2, Some(s"Failed for '${t._1}''"))
    }
  }

  "test function calls with float" should "pass the tests" in {
    List(
      ("let identity = fn(x) { x; }; identity(5.01);", 5.01f),
      ("let identity = fn(x) { return x; }; identity(5.01);", 5.01f),
      ("let double = fn(x) { x * 2; }; double(5.01);", 10.02f),
      ("let add = fn(x, y) { x + y; }; add(5.01, 5.01);", 10.02f),
      ("let add = fn(x, y) { x + y; }; add(5.01 + 5.01, add(5.01, 5.01));", 20.04f),
      ("fn(x) { x; }(5.01)", 5.01f)
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      testFloatObject(evaluated, t._2, Some(s"Failed for '${t._1}''"))
    }
  }

  "test closures" should "pass the tests" in {
    val testData =
      """
        |let newAdder = fn(x) {
        |   fn(y) { x + y };
        |};
        |
        |let addTwo = newAdder(2);
        |addTwo(2);
        |""".stripMargin

    testIntegerObject(prepareEval(testData), 4)
  }

  "test String literals" should "pass the tests" in {
    List(
      ("\"hello, world!\"", "hello, world!"),
      ("\"hello, world12222222222!\"", "hello, world12222222222!"),
      ("\"hello, \tworld\"", "hello, \tworld"),
      ("\"hello, \nworld\"", "hello, \nworld"),
      ("\"hello, \t\t\tworld\n\n\"", "hello, \t\t\tworld\n\n")
    ) foreach { t =>
      val eval = prepareEval(t._1)
      testStringObject(eval, t._2)
    }
  }

}
