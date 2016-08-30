import barneshut.conctrees.Conc
import common.parallel

import scala.annotation.tailrec
import scala.reflect.ClassTag

/*
// sequential filter left -> right for lists
def filter[T](lst: List[T])(p: T => Boolean): List[T] = lst match {
  case x :: xs if p(x) => x :: filter(xs)(p)
  case x :: xs => filter(xs)(p)
  case Nil => Nil
}

sealed trait Tree[+T]

case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]

case class Leaf[T](elem: T) extends Tree[T]

case object Empty extends Tree[Nothing]

// tree are not good for parallelism unless they are balanced
def filter[T](t: Tree[T])(p: T => Boolean): Tree[T] = t match {
  case Node(left, right) => {
    val (l, r) = parallel(filter(left)(p), filter(right)(p))
    Node(l, r)
  }
  case Leaf(elem) => if(p(elem)) t else Empty
  case Empty => Empty
}
*/

case object Empty extends Conc[Nothing] {
  def level = 0
  def size = 0
  def left = Empty
  def right = Empty
}
class Single[T](val x: T) extends Conc[T] {
  def level = 0
  def size = 1
  def left = Empty
  def right = Empty
}
case class <>[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  val level = 1 + math.max(left.level, right.level)
  val size = left.size + right.size
}

case class Append[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  val level = 1 + math.max(left.level, right.level)
  val size = left.size + right.size
}

def appendLeaf[T](xs: Conc[T], y: T): Conc[T] = Append(xs, new Single(y))

@tailrec
private def append[T](xs: Append[T], ys: Conc[T]): Conc[T] = {
  if (xs.right.level > ys.level) new Append(xs, ys)
  else {
    val zs = new <>(xs.right, ys)
    xs.left match {
      case ws @ Append(_, _) => append(ws, zs)
      case ws if ws.level <= zs.level => ws <> zs
      case ws => new Append(ws, zs)
    }
  }
}

sealed trait Conc[+T] {
  def level: Int

  def size: Int

  def left: Conc[T]

  def right: Conc[T]

  def <>(that: Conc[T]): Conc[T] = {
    if (this == Empty) that
    else if (that == Empty) this
    else concat(this, that)
  }

  def concat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
    val diff = ys.level - xs.level
    if (diff >= -1 && diff <= 1) new <>(xs, ys)
    else if (diff < -1) {
      if (xs.left.level >= xs.right.level) {
        val nr = concat(xs.right, ys)
        new <>(xs.left, nr)
      } else {
        val nrr = concat(xs.right.right, ys)
        if (nrr.level == xs.level - 3) {
          val nl = xs.left
          val nr = new <>(xs.right.left, nrr)
          new <>(nl, nr)
        } else {
          val nl = new <>(xs.left, xs.right.left)
          val nr = nrr
          new <>(nl, nr)
        }
      }
    }
    else Empty
  }
}
