package timeusage

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import timeusage.TimeUsage.classifiedColumns

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {

  test("classifiedColumns should parse correctly") {

    val columnNames = List("t01abc", "t03def", "t11adsd2", "t1801akjdhas", "t1803hbasd", "t0512321", "t18051231231", "t0221412", "t04asd", "t06123", "t07asd", "t08123", "t09asd", "t10123", "t12afsd", "t13435", "t14asd", "t155123", "t16sad", "t18123", "random", "other")
    val (primary, working, other) = classifiedColumns(columnNames)

    assert(primary.size === 5)
    assert(working.size === 2)
    assert(other.size === 13)
  }
}
