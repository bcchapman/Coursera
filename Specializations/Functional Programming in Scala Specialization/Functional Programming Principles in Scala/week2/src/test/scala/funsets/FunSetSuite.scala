package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val s4: Set = (elem: Int) => (elem >= 17 && elem < 20) // 17-19
    val s5:  Set = (elem: Int) => (elem >= 19 && elem < 22) // 19-21

    val even: Set = (elem: Int) => (elem % 2 == 0)

    val mapInput: Set = (elem: Int) => List(1,3,4,5,7,1000).contains(elem)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton contains 1")
      assert(!contains(s1, 2), "Singleton doesn't contain 2")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains all common elements within each set") {
    new TestSets {
      val s = intersect(s4, s5)
      assert(!contains(s, 18), "Intersect 18")
      assert(contains(s, 19), "Intersect 19")
      assert(!contains(s, 20), "Intersect 20")
    }
  }

  test("diff contains all elements in first set but not second") {
    new TestSets {
      val s = diff(s4, s5)
      assert(contains(s, 17), "Difference 17")
      assert(contains(s, 18), "Intersect 18")
      assert(!contains(s, 19), "Intersect 19")
      assert(!contains(s, 20), "Intersect 20")
      assert(!contains(s, 21), "Intersect 21")
    }
  }

  test("filter contains all matching elements of set") {
    new TestSets {
      val s = filter(s4, x => (x % 2 ==0))
      assert(!contains(s, 17), "Filter 17")
      assert(contains(s, 18), "Filter 18")
      assert(!contains(s, 19), "Filter 19")
    }
  }

  test("forAll contains all matching elements of set") {
    new TestSets {
      val s = forall(even, x => (x % 2 == 0))
      assert(s, "All even")
      val ss = forall(even, x => (x >= -1000 && x <= 1000))
      assert(ss, "All bounded even")
      val sss = forall(even, x => (x > 0))
      assert(!sss, "Includes negatives as well")
    }
  }

  test("exists contains some matching elements of set") {
    new TestSets {
      val s = exists(even, x => (x == 4))
      assert(s, "exists 4")
      val ss = exists(even, x => (x == 5))
      assert(!ss, "Doesn't exist 5")
      val sss = exists(even, x => (x == 2000))
      assert(!sss, "Doesn't exist 2000, unbounded")
    }
  }

  test("map contains some elements mapped from set") {
    new TestSets {
      val s = map(mapInput, x => x-1)
      assert(setToString(s).equals("{0,2,3,4,6,999}"))
    }
  }

  def setToString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }


}
