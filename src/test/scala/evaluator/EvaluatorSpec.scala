// (c) 2021 Mahesh Pujari
// This code is licensed under MIT license (see LICENSE for details)

package evaluator

import aabstract.AbstractBaseSpec
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

}
