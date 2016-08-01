package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("compute delta of (2,4,5)") {
    val result = Polynomial.computeDelta(Signal(2), Signal(4), Signal(5))
    assert(result() === -24)
  }

  test("compute delta of (-2,4,-5)") {
    val result = Polynomial.computeDelta(Signal(-2), Signal(4), Signal(-5))
    assert(result() === -24)
  }

  test("eval of Literal") {
    val result = Calculator.eval(new Literal(12), Map())
    assert(result === 12)
  }

  test("eval of Unknown ref") {
    val result = Calculator.eval(new Ref("unknown"), Map())
    assertNan(result)
  }

  test("eval of Literal ref") {
    val result = Calculator.eval(new Ref("a"), Map("a" -> Signal(Literal(12))))
    assert(result === 12)
  }

  test("eval of plus ") {
    val result = Calculator.eval(new Plus(Literal(12), Literal(4)), Map())
    assert(result === 16)
  }

  test("eval of minus ") {
    val result = Calculator.eval(new Minus(Literal(12), Literal(4)), Map())
    assert(result === 8)
  }

  test("eval of times ") {
    val result = Calculator.eval(new Times(Literal(12), Literal(4)), Map())
    assert(result === 48)
  }

  test("eval of divide ") {
    val result = Calculator.eval(new Divide(Literal(12), Literal(4)), Map())
    assert(result === 3)
  }

  test("eval of nested evals with refs") {
    val a = new Times(Literal(12), Literal(4))
    val b = new Plus(Literal(12), Literal(4))

      val result = Calculator.eval(new Minus(new Ref("a"), new Ref("b")), Map("a" -> Signal(a), "b" -> Signal(b)))
      assert(result === 32)
  }

  test("cyclic detection") {
    val a = new Ref("b")
    val b = new Ref("a")

    val result = Calculator.eval(new Plus(new Ref("a"), new Ref("b")), Map("a" -> Signal(a), "b" -> Signal(b)))
    assertNan(result)
  }

  // a = 12 + c
  // b = 2 + c
  // c = a
  test("neset cyclic detection") {
    val a = new Plus(Literal(12), new Ref("c"))
    val b = new Plus(new Literal(2), new Ref("c"))
    val c = new Ref("a")

    val result = Calculator.eval(new Plus(a, b), Map("a" -> Signal(a), "b" -> Signal(b), "c" -> Signal(c)))
    assertNan(result)
  }

  def assertNan(d: Double) = assert(d.toString === Double.NaN.toString)
}
