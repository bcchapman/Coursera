package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balance(chars: Array[Char], weight: Int): Boolean = {
      if(chars.isEmpty) weight == 0  //reached end
      else if(weight < 0) false // Out of balance detected
      else {
        def curChar = chars.head
        if (curChar == '(') balance(chars.tail, weight + 1)
        else if (curChar == ')') balance(chars.tail, weight - 1)
        else balance(chars.tail, weight)
      }
    }

    balance(chars, 0)
  }

  type BalanceResult = (Int, Int)
  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): BalanceResult = {
      def traverseAcc(input: Array[Char], acc: BalanceResult): BalanceResult = {
        if(input isEmpty) acc
        // left paren found, always add
        else if (input.head == '(') traverseAcc(input.tail, (acc._1 + 1, acc._2))
        else if (input.head == ')') {
          // decrement running left counter if we have any
          if(acc._1 > 0) traverseAcc(input.tail, (acc._1 - 1, acc._2))
          // otherwise increment right
          else traverseAcc(input.tail, (acc._1, acc._2 + 1))
        }
        // regular char, continue
        else traverseAcc(input.tail, acc)
      }

      traverseAcc(chars.slice(idx, until), (arg1, arg2))
    }

    def reduce(from: Int, until: Int): BalanceResult = {
      if(until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + (until - from) / 2
        val (r1, r2) = parallel(reduce(from, mid), reduce(mid, until))
        val minMatch = Math.min(r1._1, r2._2)
        (r1._1 + r2._1 - minMatch, r1._2 + r2._2 - minMatch)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
