package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  def checkParBalance(input: String, threshold: Int, expected: Boolean) =
    assert(parBalance(input.toArray, threshold) == expected,
      s"balance($input) should be $expected")

  test("parBalance should work for nested parentheses and threshold 1") {
    checkParBalance("()).", 1, false)
  }

  test("parBalance should work for string of length 2 and threshold 1") {
    checkParBalance("')('", 1, false)
  }

  test("parBalance should work for nested parentheses and threshold 1 for true") {
    checkParBalance("(())", 1, true)
  }

  test("parBalance should work for string of length 2 and threshold 1 for true") {
    checkParBalance("()", 1, true)
  }
}