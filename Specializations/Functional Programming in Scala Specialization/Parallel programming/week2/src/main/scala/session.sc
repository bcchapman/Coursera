import common.parallel

object parallelMap {

  val threshold = 4
  def mapASegSeq[A, B](inp: Array[A], left: Int, right: Int, f: A => B,
                       out: Array[B]) = {
    var i = left
    while (i < right) {
      out(i) = f(inp(i))
      i = i + 1
    }
  }

  def mapASegPar[A, B](inp: Array[A], left: Int, right: Int, f: A => B,
                       out: Array[B]): Unit = {
    // Writes to out(i) for left <= i <= right - 1
    if (right - left < threshold) {
      mapASegSeq(inp, left, right, f, out)
    }
    else {
      val mid = left + (right - left) / 2
      parallel(mapASegPar(inp, left, mid, f, out),
        mapASegPar(inp, mid, right, f, out))
    }
    var i = left
    while (i < right) {
      out(i) = f(inp(i))
      i = i + 1
    }
  }

  val range = Range.inclusive(0, 20)
  val in = range.toArray
  val out = range.map(x => 0).toArray
  val f = (x: Int) => x * x
  mapASegPar(in, 5, 15, f, out)
  out.foreach(println)
}

parallelMap