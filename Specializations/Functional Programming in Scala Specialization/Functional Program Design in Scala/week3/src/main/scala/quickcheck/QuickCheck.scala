package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop.{apply => _, _}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =  for {
    k <- arbitrary[A]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("findMinInsertDuplicate") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMinDeleteInsertDuplicate") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, deleteMin(h))) == m
  }

  property("EmptyInsert2Min") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    val m2 = if (m >= 0) m - 1 else m + 1 // keep from crossing Int bound

    findMin(insert(m, insert(m2, emptyHeap(h)))) == Math.min(m, m2)
  }

  property("EmptyInsertDeleteEmpty") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    isEmpty(deleteMin(insert(m, emptyHeap(h))))
  }

  property("OrderCorrectRemoval") = forAll { (h: H) =>
   def checkOrder(h: H, prevVal: A): Boolean = h match {
     case _ if (isEmpty(h)) => true
     case _ => prevVal <= findMin(h) && checkOrder(deleteMin(h), findMin(h))
   }

    if(isEmpty(h)) true
    else {
      val prevVal = findMin(h)
      checkOrder(deleteMin(h), prevVal)
    }
  }

  property("MinimumOfMeld") = forAll { (h: H) =>
    val h2 = deleteMin(h)

    findMin(meld(h, h2)) == findMin(h)
  }

  property("InsertExists") = forAll { (h: H) =>
    def searchForValue(searchVal: A, h: H): Boolean = h match {
      case _ if (isEmpty(h)) => false
      case _ => findMin(h) == searchVal || searchForValue(searchVal, deleteMin(h))
    }
    val m = if (isEmpty(h)) 0 else findMin(h)
    val m2 = m / 2

    searchForValue(m2, insert(m2, h))
  }

  property("InsertMin") = forAll { (h: H) =>
    findMin(insert(Integer.MIN_VALUE, h)) == Integer.MIN_VALUE
  }

  property("InsertMax") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(Integer.MAX_VALUE, h)) == m
  }

  def emptyHeap(h: H): H = h match {
    case _ if (isEmpty(h)) => h
    case _ => emptyHeap(deleteMin(h))
  }
}
