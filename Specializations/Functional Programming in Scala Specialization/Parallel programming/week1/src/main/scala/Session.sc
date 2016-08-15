import com.sun.jnlp.JNLPRandomAccessFileNSBImpl

object HelloThread {

  case class HelloThread(index: Int) extends Thread {

    override def run(): Unit = {
      println(s"Hello World - $index!")
    }
  }

  val threads = for {
    i <- 1 to 3
  } yield HelloThread(i)

  //start all threads
  threads.foreach(t => {
    t.start
  })

  //wait for all to finish
  threads.foreach(t => {
    t.join
  })
}
// HelloThread

// atomicity
object Atomicity {
  private val x = new AnyRef {}
  private var uidCount = 0L

  def getUniqueId(): Long = x.synchronized {
      uidCount = uidCount + 1
      uidCount
  }

  def startThread() = {
    val t = new Thread {
      override def run() = {
        val uids = for {
          i <- 0 until 10
        } yield getUniqueId()
        println(uids)
      }
    }
    t.start
    t
  }

  val t1 = startThread()
  val t2 = startThread()

  Thread.sleep(1000)
}
// Atomicity

object sumSegment {
  import common.parallel

  def power(x: Int, p: Double) = math.exp(p * math.log(math.abs(x))).toInt

  def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {
    val segment = for {
      i <- s until t
    } yield power(a(i), p)
    segment.reduce(_+_)
  }

  def pNorm(a: Array[Int], p: Double) = power(sumSegment(a, p, 0, a.length), 1/p)

  def pNormTwoPart(a: Array[Int], p: Double) = {
    val m = a.length / 2
    val (sum1, sum2) = parallel(sumSegment(a, p, 0, m), sumSegment(a, p, m, a.length))
    power(sum1 + sum2, 1/p)
  }

  def threshold = 4
  def pNormRec(a: Array[Int], p: Double): Int = power(segmentRec(a, p, 0, a.length), 1/p)
  def segmentRec(a: Array[Int], p: Double, s: Int, t: Int): Int = {
    if ( t - s < threshold)
      sumSegment(a, p, s, t)
    else {
      val m = s + (t - s) / 2
      val (sum1, sum2) = parallel(segmentRec(a, p, s, m), segmentRec(a, p, m, t))
      sum1 + sum2
    }
  }

  val res = sumSegment(List(5,1,4,3,2).toArray, 2, 0, 3)
  println(res)

  val res1 = pNorm(List(5,1,4,3,2).toArray, 2)
  println(res1)

  val res2 = pNormTwoPart(List(5,1,4,3,2).toArray, 2)
  println(res2)

  val res3 = pNormRec(List(5,1,4,3,2).toArray, 2)
  println(res3)


}
// sumSegment

object monteCarlo {
  import scala.util.Random
  import common.parallel

  def mcCount(iter: Int): Int = {
    val randomX = Random
    val randomY = Random
    var hits = 0
    for (i <- 0 until iter) {
      val x = randomX.nextDouble
      val y = randomY.nextDouble
      if(x*x + y*y < 1) hits = hits + 1
    }
    hits
  }
  def monteCarloPiSeq(iter: Int): Double = 4.0 * mcCount(iter) / iter

  def monteCarloPiPar(iter: Int): Double = {
    val (p1, p2, p3, p4) = parallel(
      mcCount(iter/4), mcCount(iter/4),
      mcCount(iter/4), mcCount(iter - 3 * (iter/4)))
      4.0 * (p1 + p2 + p3 + p4) / iter
  }

  println(monteCarloPiSeq(8))
  println(monteCarloPiPar(200000))
}

monteCarlo



