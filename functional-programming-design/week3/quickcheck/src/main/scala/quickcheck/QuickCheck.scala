package quickcheck

import common._

import org.scalacheck._
import Gen._
import Prop._
import Arbitrary._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v,h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    def min(x: Int, y: Int): Int = if(x < y) x else y
    val g = insert(a, empty)
    val h = insert(b, g)
    val x = findMin(h) 
    val y = min(a,b)
    x == y
  }

  property("delMin") = forAll { a: Int =>
    val g = insert(a, empty)
    val e = deleteMin(g)
    e == empty
  }

  property("deleteMin") = forAll { (x: A, y: A) => 
    val heap =  insert(x, insert(y, empty))
    if(ord.lteq(x,y)) findMin(deleteMin(heap)) == y
    else findMin(deleteMin(heap)) == x
  }
  
  property("deleteMin") = forAll { (x: A, y: A) => 
    val heap =  insert(x, insert(y, empty))
    if(ord.lteq(x,y)) {
      val m = insert(y, insert(x, insert(x, insert(x, empty))))
      findMin(deleteMin(deleteMin(deleteMin(m)))) == y
    }
    else {
      val m = insert(x, insert(y, insert(y, insert(y, empty))))
      findMin(deleteMin(deleteMin(deleteMin(m)))) == x
    }
  }

  property("deleteMin") = forAll { (x: A, y: A) => 
    val heap =  insert(x, insert(y, empty))
    if(ord.lteq(y,x)) {
      val m = insert(x, insert(x, insert(x, insert(y, empty))))
      findMin(m) == y
    }
    else {
      val m = insert(y, insert(y, insert(y, insert(x, empty))))
      findMin(m) == x
    }
  }

  property("isSorted") = forAll { (h: H) =>
    def isSorted(m: A, s: H): Boolean = {
      if (isEmpty(s)) true
      else {
        val m1 = findMin(s)
        if (ord.lteq(m,m1)) isSorted(m1,deleteMin(s))
        else false
      }
    }
    
    if(isEmpty(h)) true 
    else isSorted(findMin(h), deleteMin(h))
  }

  property("melded heaps") = forAll { (h: H, h1: H) =>
    (isEmpty(h), isEmpty(h1)) match {
      case (true, true) => true
      case (true, false) => 
        val min = findMin(h1)
        findMin(meld(h, h1)) == min

      case (false, true) =>
        val min = findMin(h)
        findMin(meld(h, h1)) == min
      
      case (false, false) => 
        val minH = findMin(h)
        val minH1 = findMin(h1)

        val newHeap = meld(h,h1)
        val minNewHeap = findMin(newHeap)
        (minNewHeap == minH || minNewHeap == minH1)
    }
  }
}
