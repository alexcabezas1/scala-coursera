package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("hint1") = forAll { (a: A, b: A) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == ord.min(a, b)
  }

  property("hint2") = forAll { a: A =>
    deleteMin(insert(a, empty)) == empty
  }

  property("hint3") = forAll { h: H =>
    def mins(h2: H): List[A] =
      if (isEmpty(h2)) Nil
      else {
        findMin(h2) :: mins(deleteMin(h2))
      }

    def isSorted(l: List[A]): Boolean = l match {
      case Nil => true
      case x :: Nil => true
      case x :: xs => ord.lteq(x,xs.head) && isSorted(xs)
    }

    isSorted(mins(h)) == true
  }

  property("hint4") = forAll { (h1: H, h2: H) =>
    List(findMin(h1), findMin(h2)) contains findMin(meld(h1, h2))
  }

  property("meld1") = forAll { h1:H =>
    meld(h1, empty) == meld(empty, h1)
  }

  property("meld2") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  property("meld3") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val min = m1.min(m2)
    findMin(meld(deleteMin(h1),insert(min,h2)))==min
  }
}
