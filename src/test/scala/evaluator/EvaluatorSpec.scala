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

}
