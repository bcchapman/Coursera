class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x,y)

  def numer = x / g
  def denom = y / g

  def + (that: Rational): Rational =
    new Rational(
      this.numer * that.denom + that.numer * this.denom,
      this.denom * that.denom)

  def - (that: Rational): Rational =
    this + -that

  def < (that: Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational): Rational = if (this < that) that else this

  def unary_- : Rational =
    new Rational(-numer, denom)

  override def toString = numer + "/" + denom
}

(new Rational(1,2) + new Rational(2,3)).toString

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.toString() + " => " + (-x).toString
x + y
x - y - z

x < y
y.max(x)
new Rational(4, 8).toString