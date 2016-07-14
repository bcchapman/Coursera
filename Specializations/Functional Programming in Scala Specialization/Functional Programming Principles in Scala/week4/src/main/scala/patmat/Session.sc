/*
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  override def toString = head.toString + " " + tail.toString
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def toString = ""
}

object List {
  def apply[T] (x1: T, x2: T) : List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply[T] (x1: T) : List[T] = new Cons(x1, new Nil)
  def apply[T]() = new Nil

}

List().toString
List(1).toString
List(2,3).toString
*/

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }

  def show: String = this match {
    case Number(n) => n.toString
    case Var(s) => s
    case Sum(e1, e2) => e1.show + " + " + e2.show
    case Prod(Sum(e1,e2), e3) => "(" + e1.show + " + " + e2.show + ")" + " * " + e2.show
    case Prod(e1, Sum(e2,e3)) => e1.show + " * " + "(" + e2.show + " + " + e3.show + ")"
    case Prod(e1, e2) => e1.show + " * " + e2.show
  }
}

case class Number(n: Int) extends Expr
case class Var(n: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

Sum(Prod(Number(2), Var("x")), Var("y")).show

Prod(Sum(Number(2), Var("x")), Var("y")).show
Prod(Var("y"), Sum(Number(2), Var("x"))).show


val fruit = List("apples", "oranges", "pears")
val diag3: List[List[Int]] = List(List(1,0,0), List(0,1,0), List(0,0,1))

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys if (x > y) => x :: y :: ys
  case y :: ys => y :: insert(x, ys)
}

def unsorted = List(7, 3, 9, 2)
unsorted.toString
isort(unsorted).toString