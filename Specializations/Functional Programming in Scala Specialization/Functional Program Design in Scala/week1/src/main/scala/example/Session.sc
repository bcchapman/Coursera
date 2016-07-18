import java.util.Random

val f: PartialFunction[String, String] = { case "ping" => "pong"}
f("ping")
f.isDefinedAt("abc")

val f2: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: y :: rest => "two"
}

f2.isDefinedAt(List(1,2,3))

val f3: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: rest => "two"
}

f3.isDefinedAt(List(1))


case class Book(title: String, authors: List[String])

val books: Set[Book] = Set(
  Book(title = "Structure and Interpretation of Computer Programs",
    authors = List("Abelson, Harald", "Sussman, Gerald J.")),
  Book(title = "Introduction to Functional Programming",
    authors = List("Bird, Richard", "Wadler, Phil")),
  Book(title = "Effective Java",
    authors = List("Bloch, Joshua")),
  Book(title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),
  Book(title = "Programming in Scala",
    authors = List("Ordersky, Martin", "Spoon, Lex", "Venners, Bill", "Bloch, Joshua"))
)

for (b <- books; a <- b.authors if a startsWith "Bloch,")
  yield b.title

for (b <- books if (b.title indexOf "Program") >= 0)
  yield b.title

for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1


for ( b <- books; a <- b.authors if a startsWith "Bird")
  yield b.title

books flatMap (b => b.authors withFilter (a => a startsWith "Bird") map (y => b.title))

trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate = f(self.generate).generate
  }
}

/* Random Generators */
val integers = new Generator[Int] {
  val rand = new Random
  def generate = rand.nextInt
}

val booleans = for (x <- integers) yield x > 0

def pairs[T, U](t: Generator[T], u: Generator[U]) = new Generator[(T,U)] {
  def generate = (t.generate, u.generate)
}

def single[T](x: T): Generator[T] = new Generator[T] {
  def generate = x
}

def choose(lo: Int, hi: Int): Generator[Int] =
   for (x <- integers) yield lo + Math.abs(x % (hi - lo))

def oneOf[T](xs: T*): Generator[T] =
  for(idx <- choose(0, xs.length)) yield xs(idx)

def emptyLists = single(Nil)
def nonEmptyLists =  for {
  head <- integers
  tail <- lists
} yield head :: tail

def lists: Generator[List[Int]] = for {
  isEmpty <- booleans
  list <- if(isEmpty) emptyLists else nonEmptyLists
} yield list


lists.generate

trait Tree
case class Inner(left: Tree, right: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def leafs: Generator[Leaf] = {
  for (x <- integers) yield Leaf(x)
}

def inners: Generator[Inner] = for {
  l <- trees
  r <- trees
} yield Inner(l,r)

def trees: Generator[Tree] = for {
  isLeaf <- booleans
  tree <- if(isLeaf) leafs else inners
} yield tree

trees.generate

def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
  for(i <- 0 until numTimes) {
    val value = g.generate
    assert(test(value), "test failed for " + value)
  }
}

test(pairs(lists, lists)) {
  case (xs,ys) => (xs ++ ys length) >= xs.length
}