def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

def nthPrime(n: Int) = ((1000 to 10000).toStream filter isPrime)(n)

nthPrime(2)
nthPrime(5)

def streamRange(lo: Int, hi: Int): Stream[Int] =
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))

def listRange(lo: Int, hi: Int): List[Int] =
  if (lo >= hi) Nil
  else lo :: listRange(lo + 1, hi)

streamRange(0, 1000000000)

// Stack overflow
// listRange(0, 1000000000)

def expr: Unit = {
  val x ={print("x"); 1}
  lazy val y ={print("y"); 2}
  def z ={print("z"); 3}
  z + y + x + z + y + x
}
expr

def from(n: Int): Stream[Int] = n #:: from(n+1)

val nats = from(0)
val m4s = nats map (_ * 4)

(m4s take 15) toList

def sieve(s: Stream[Int]): Stream[Int] = {
  s.head #:: sieve(s.tail filter (_ % s.head != 0))
}

val primes = sieve(from(2))
primes.take(15) toList

def isGoodEnough(guess: Double, x: Double) =
  math.abs((guess * guess - x) / x) < 0.0001

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}

(sqrtStream(4) filter (isGoodEnough(_, 4)) take 1) toList

