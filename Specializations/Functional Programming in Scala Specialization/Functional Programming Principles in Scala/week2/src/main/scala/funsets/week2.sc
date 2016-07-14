object ScratchPad {
  def sum(f: Int => Int) : (Int, Int) => Int = {
    def sumF(x: Int, y: Int): Int = {
      if (x > y) 0
      else f(x) + sumF(x + 1, y)
    }
    sumF
  }

  def sumSugar(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f)(a + 1, b)

  def fact(n: Int): Int =
    if(n == 0) 1 else n * fact(n-1)

  def sumInts = sum(x => x)
  def sumCubes = sum(x => x*x*x)
  def sumFactorials = sum(fact)

  sumInts(1, 5)
  sumSugar(x => x)(1,5)
  sumCubes(1, 5)
  sumFactorials(1, 5)
}

object Excercise1 {

  import ScratchPad.fact, ScratchPad.sumInts

  //Exercise 1
  def product(f: Int => Int)(a: Int, b: Int): Int = if (a > b) 1 else f(a) * product(f)(a + 1, b)

  def factImproved(n: Int) = product(x => x)(1, n)

  product(x => x * x)(3, 4)
  fact(3)
  factImproved(3)
  sumInts(1, 5)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

  def productImproved(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x, y) => x * y, 1)(a, b)

  def factImproved2(n: Int) = mapReduce(x => x, (x, y) => x * y, 1)(1, n)

  def sumIntsImproved(a: Int, b: Int) = mapReduce(x => x, (x, y) => x + y, 0)(a, b)

  product(x => x * x)(3, 4)
  fact(3)
  factImproved2(3)
  sumIntsImproved(1, 5)
}

object Excercise2 {

  import Math.abs

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  fixedPoint(x => 1 + x / 2)(1)

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1.0)

  sqrt(9)
}
