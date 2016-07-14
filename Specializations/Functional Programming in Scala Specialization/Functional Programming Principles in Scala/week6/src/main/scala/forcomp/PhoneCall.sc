import scala.io.Source

val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

val words = in.getLines.toList filter (word => word forall (ch => ch.isLetter))

val mnem = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

/** Invert the mnem map to give a map from chars 'A' to 'Z' to '2' to '9' **/
var charCode: Map[Char, Char] = {
  for {
    (dig, str) <- mnem
    letter <- str
  } yield letter -> dig
}

/** Maps a word to the digit string it can represent, e.g. "Java" -> "5282" */
def wordCode(word: String): String = (word toUpperCase) map (charCode)

/** A map from digit strings to the words that represent them,
  * e.g. "5282" -> List("Java", "Kata", "Lava", ....)
  * Note: A missing number should map to the empty set, e.g. "1111" -> List()
  */
val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue List()

def encode(number: String): Set[List[String]] =
  number match {
    case n if(n.isEmpty) => Set(List())
    case _ => {
      for {
        depth <- 1 to number.size
        word <- wordsForNum(number take depth)
        rest <- encode(number drop depth)
      } yield word :: rest
    }.toSet
  }

val res = encode("7225247386")

def translate(number: String): Set[String] =
  encode(number) map (_ mkString " ")


translate("7225247386")

