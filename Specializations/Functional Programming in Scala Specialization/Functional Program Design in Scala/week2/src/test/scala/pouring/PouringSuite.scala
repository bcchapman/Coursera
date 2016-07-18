package pouring

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PouringSuite extends FunSuite {

  trait PouringTest {
    def pouring = new Pouring(Vector(4, 7))
  }

  test("moves size for Vector(4,7)") {
    new PouringTest() {
      assert(pouring.moves.size == 6)
    }
  }

  test("first 3 path sets for Vector(4,7)") {
    new PouringTest() {
      assert(pouring.pathSets.take(3).size == 3)
    }
  }

  test("there are two solutions of 6 for Vector(4,7)") {
    new PouringTest() {
      assert(pouring.solutions(6).size === 2)
    }
  }

  test("there are no solutions of 8 for Vector(4,7) but does terminate") {
    new PouringTest() {
      assert(pouring.solutions(8).size === 0)
    }
  }
}