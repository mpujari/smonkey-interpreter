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
      testBooleanObject(evaluated, t._2)
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
      ("(10 + 2) * 30 == 300 + 20 * 3", true),
      ("(5 > 5 == true) != false", false),
      ("500 / 2 != 250", false),
      ("500 / 2 == 250", true),
      ("5 * 10 > 40 + 5", true),
      ("5 * 10 < 40 + 5", false),
      ("3 + 4 * 5 == 3 * 1 + 4 * 5", true),
      ("3 + 4 * 5 != 3 * 1 + 4 * 5", false),
      ("3 + 4 * 5 <= 3 * 1 + 4 * 5", true),
      ("3 + 4 * 5 >= 3 * 1 + 4 * 5", true),
      ("3 + 4 * 5 < 3 * 1 + 4 * 5", false),
      ("3 + 4 * 5 > 3 * 1 + 4 * 5", false),
      ("3 + 4 * 5 > 3 * 1 + 4 * 5", false),
      ("3 + 4 * 5 < 3 * 1 + 4 * 5", false)
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t._1)
      testBooleanObject(evaluated, t._2)
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

  "Error Handling test" should "pass the tests" in {
    List(
      ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
      ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
      ("-true", "unknown operator: -BOOLEAN"),
      ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
      ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
      ("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN"),
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

  "test function param, body" should "pass the tests" in {
    List(
      "fn(x) { x + 2; };"
    ) foreach { t =>
      val evaluated: obj.Object = prepareEval(t)
      assert(evaluated.isInstanceOf[obj.Function])
      val fn: obj.Function = evaluated.asInstanceOf[obj.Function]
      assert(fn.parameters.size == 1)
      assert(fn.parameters.head.toString == "x")
      assert(fn.body.toString == "(x + 2)")
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

}
