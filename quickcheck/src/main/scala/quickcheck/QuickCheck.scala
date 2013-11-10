package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = Gen.sized { size =>
    for {
      ns <- Gen.listOfN(size, Gen.choose(-1000, 1000))
    } yield ns.foldLeft(empty)((h, m) => insert(m, h))
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == math.min(a, b)
  }

  property("empty-ins-del-empty") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  def isSorted(ns: List[Int]): Boolean = {
    def loop(ns: List[Int], soFar: Int): Boolean = ns match {
      case Nil   => true
      case n::ns => if (n >= soFar) loop(ns, n)
      else false
    }
    loop(ns, Int.MinValue)
  }

  def contents(h: H): List[Int] = {
    def loop(h: H, accum: List[Int]): List[Int] =
      if (isEmpty(h)) accum.reverse
      else {
        val m = findMin(h)
        loop(deleteMin(h), m::accum)
      }
    loop(h, Nil)
  }

  property("contents-sorted") = forAll { h: H =>
    isSorted(contents(h))
  }

  property("readding-contents-same-contents") = forAll { h: H =>
    val c = contents(h)
    val hh = c.foldLeft(empty)((h, m) => insert(m, h))
    contents(hh) == c
  }

  property("melded-minimum") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) true
    else {
      val mm = findMin(meld(h1, h2))
      (!isEmpty(h1) && mm == findMin(h1)) || (!isEmpty(h2) && mm == findMin(h2))
    }
  }
}
