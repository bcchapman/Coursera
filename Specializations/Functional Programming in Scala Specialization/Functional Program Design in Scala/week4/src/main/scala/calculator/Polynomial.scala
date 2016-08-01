package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal((b() * b()) - (4 * (a() * c())))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Signal {
      val intDis = computeDelta(a, b, c)() // intermediary discriminant
      intDis match {
        case id if (id <= 0) => Set()
        case id => Set((-b() + Math.sqrt(id)) / (2 * a()), (-b() - Math.sqrt(id)) / (2 * a()))
      }
    }
  }
}

