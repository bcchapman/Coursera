package streams

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StringParserTerrainSuite extends FunSuite with Matchers{

  trait TerrainTester extends GameDef with StringParserTerrain  {
      override val level: String =
        """So-
          |ooo
          |oTo""".stripMargin
  }

  test("startPos") {
    new TerrainTester() {
      startPos shouldEqual Pos(0,0)
    }
  }

  test("goal") {
    new TerrainTester() {
      goal shouldEqual Pos(2,1)
    }
  }

  test("valid terrain") {
    new TerrainTester {
      terrain(Pos(2, 2)) shouldBe true
    }
  }

  test("invalid value terrain") {
    new TerrainTester {
      terrain(Pos(0,2)) shouldBe false
    }
  }

  test("invalid row terrain") {
    new TerrainTester {
      terrain(Pos(3,2)) shouldBe false
    }
  }

  test("invalid col terrain") {
    new TerrainTester {
      terrain(Pos(2,3)) shouldBe false
    }
  }

}
