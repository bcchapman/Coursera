import scala.collection.GenSeq

def initializeArray(xs: Array[Int])(v: Int) =
  for(i <- (0 until xs.length).par) {
    xs(i) = v
  }

def sum(xs: Array[Int]) =
  xs.par.foldLeft(0)( _ + _) // does not execute in parallel

sum(Array(1,2,3))

// def fold(z: A)(f: (A,A) => A): A
def sumPar(xs: Array[Int]) =
  xs.par.fold(0)( _ + _)

sumPar(Array(1,2,3))

def maxPar(xs: Array[Int]) =
  xs.par.fold(Int.MinValue)(Math.max)

maxPar(Array(1,2,8,3))

// not associative
Array("paper","rock","paper","scissors")
  .par.fold("")(play)

def play(a: String, b: String): String = List(a,b).sorted match {
  case List("paper", "scissors") => "scissors"
  case List("paper", "rock") => "paper"
  case List("rock", "scissors") => "rock"
  case List(a, b) if (a == b) => a
  case List("", b) => b
}

val vowels = List("A", "E", "I", "O", "U", "Y") // sometimes Y
def isVowel(c: String): Boolean = vowels.contains(c.toUpperCase)

Array("E","P","F","L").par.aggregate(0)(
  (count, c) => if(isVowel(c)) count + 1 else count, _ + _
)

def largestPalindrome(xs: GenSeq[Int]): Int = {
  xs.aggregate(Int.MinValue)(
    (largest, n) =>
      if(n > largest && n.toString == n.toString.reverse) n else largest,
        Math.max
  )
}

val array = (0 until 1000000).toArray
largestPalindrome(array.par)
