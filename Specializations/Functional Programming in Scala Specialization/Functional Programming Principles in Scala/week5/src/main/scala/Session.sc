import math.Ordering

def mSort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val (fst, snd) = xs splitAt n
    merge(mSort(fst), mSort(snd))
  }
}

val inputs = List(2, -4, 5, 7, 1)
val fruits = List("apple", "pineapple", "orange", "banana")
mSort(inputs)
mSort(fruits)

def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => (y * y) :: squareList(ys)
  }

def squareList2(xs: List[Int]): List[Int] =
  xs map (x => x*x)

def nums = List(1,2,3,-4,4,5)
squareList(nums)
squareList2(nums)

nums.filter( _ > 0)
nums.filterNot( _ > 0)
val (left, right) = nums partition (_ > 0)

nums takeWhile( _ > 0)
nums dropWhile( _ > 0)
val (l2, r2) = nums span (_ > 0)

val data = List("a","a","a","b","c","c","a")

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs span (_.equals(x))
    first :: pack(rest)
  }
}
def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (l => (l.head, l.length))

pack(data)
encode(data)

def sum(xs: List[Int])     = (0 :: xs) reduceLeft (_ + _)
def prod(xs: List[Int])    = (1 :: xs) reduceLeft (_ * _)
def sumImp(xs: List[Int])  = (xs foldLeft 0)(_ + _)
def prodImp(xs: List[Int]) = (xs foldLeft 1)(_ * _)

sum(nums)
sumImp(nums)
prod(nums)
prodImp(nums)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())((t, us) => f(t) :: us)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (t, us) => xs.length )

def m(v:Int): Int = (v*2)

mapFun(nums, m)
lengthFun(nums)