object Queens {
val xs  = Array(1,2,3,44)
xs map (x => x * 2)

val s = "Hello World"
s filter (s => s.isUpper)
s exists (c => c.isUpper)
s forall (c => c.isUpper)

val pairs = List(1, 2, 3) zip s
pairs unzip

s flatMap(c => List('.', c))

val nums = List(1,2,3)
nums.sum
nums.max

def combos (M: Int, N: Int) =
  (1 to M) flatMap ( x => (1 to N) map (y => (x, y)))
combos(5, 3)

def scalarProducts(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{ case (x, y) => x * y }.sum

def scalarProducts2(xs: Vector[Double], ys: Vector[Double]): Double =
  (for {
    (x,y) <- xs zip ys
  } yield x * y).sum


def isPrime(n: Int): Boolean = (2 until n) forall (i => n % i != 0)

isPrime(3)
isPrime(4)

def something(n: Int) =
  (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j))) filter (pair =>
      isPrime(pair._1 + pair._2))

def something2(n: Int) =
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i+j)
  } yield (i,j)

something(7)
something2(7)

case class Person(name: String, age: Int)
val persons: List[Person] = List()
for (p <- persons if p.age > 20) yield p.name

def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
  if (k == 0) Set(List())
  else
    for {
      queens <- placeQueens(k - 1)
      col <- 0 until n
      if isSafe(col, queens)
    } yield col :: queens

  placeQueens(n)
}

/**
  * Determines whether a valid placement has been made
  * 1) No other queen sharing row. This is assumed to be true
  * 2) No other queen sharing column.
  * 3) No other queen in diagonal space
  *
  * @param col
  * @param queens
  * @return
  */
def isSafe2(col: Int, queens: List[Int]): Boolean = {
  def isDiagonalSafe(rowsAbove: Int, queens: List[Int]): Boolean = queens match {
    case Nil => true
    case q :: qs => (q != col - rowsAbove && q != col + rowsAbove) && isDiagonalSafe(rowsAbove + 1, qs)
  }

  //Vertical. No queens in given column
  !(queens contains col) && isDiagonalSafe(1, queens)
}


def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  val queensWithRow = (row - 1 to 0 by -1) zip queens
  queensWithRow forall {
    case (r, c) => col != c && math.abs(col - c) != row -r
  }
}

def show(queens: List[Int]) = {
  val lines =
    for( col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

(queens(8) take 3 map show) foreach println
}

val romanNumerals = Map( "I" -> 1, "V" -> 5, "X" -> 10)
val capitalOfCountry = Map( "US" -> "Washington", "Switzerland" -> "Bern")
capitalOfCountry get "US"
capitalOfCountry get "Andorra"

def showCapital(country: String) = capitalOfCountry get country match {
  case Some(capital) => capital
  case None => "missing data"
}
showCapital("US")
showCapital("Andorra")

val showCapital2 = capitalOfCountry withDefaultValue("missing data")
showCapital2("US")
showCapital2("Andorra")

val fruit = List("apple", "pear", "orange", "pineapple")
fruit sortWith (_.length < _.length)
fruit.sorted
fruit groupBy (_.head)

class Poly(terms0: Map[Int, Double]) {
  val terms = terms0 withDefaultValue 0.0
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  def + (other: Poly) = terms ++ (other.terms map adjust)
  def adjust(otherTerm: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = otherTerm
    exp -> (coeff + terms(exp))
  }
  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
}

val p1 = new Poly(1 -> 2.0, 3-> 4.0, 5 -> 6.2)
val p2 = new Poly(0 -> 3.0, 3-> 7.0)
p1 + p2

class Poly2(terms0: Map[Int, Double]) {
  val terms = terms0 withDefaultValue 0.0
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  def + (other: Poly2) = new Poly2((other.terms foldLeft terms)(addTerm))
  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms (exp) ) )
  }
  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
}

val p3 = new Poly(1 -> 2.0, 3-> 4.0, 5 -> 6.2)
val p4 = new Poly(0 -> 3.0, 3-> 7.0)
p3 + p4