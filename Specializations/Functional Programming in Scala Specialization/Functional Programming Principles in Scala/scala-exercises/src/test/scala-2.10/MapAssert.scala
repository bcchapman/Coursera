import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Maps extends FlatSpec with Matchers {

  /** Maps may be accessed:
    */
  def mayBeAccessedMaps(res0: String, res1: String) {
    val myMap = Map("MI" → "Michigan", "OH" → "Ohio", "WI" → "Wisconsin", "IA" → "Iowa")
    myMap("MI") should be(res0)
    myMap("IA") should be(res1)
  }

  /** If a map key is requested using myMap(missingKey) which does not exist a NoSuchElementException will be thrown.
    * Default values may be provided using either getOrElse or withDefaultValue for the entire map
    */
  def defaultValuesMayBeProvidedMaps(res0: String, res1: String) {
    val myMap = Map("MI" → "Michigan", "OH" → "Ohio", "WI" → "Wisconsin", "IA" → "Iowa")
    intercept[NoSuchElementException] {
      myMap("TX")
    }
    myMap.getOrElse("TX", "missing data") should be(res0)

    val myMap2 = Map("MI" → "Michigan", "OH" → "Ohio", "WI" → "Wisconsin", "IA" → "Iowa") withDefaultValue "missing data"
    myMap2("VA") should be(res1)
  }


  mayBeAccessedMaps("Michigan", "Iowa")
  defaultValuesMayBeProvidedMaps("missing data", "missing data")
}