package lectures

import org.scalatest._

class Lecture2_3Spec extends FlatSpec with Matchers {
  "A fixed point of f(x) = 1 + x/2 using 1.0 as a guess" should "be 2" in {
    Lecture2_3.fixedPoint(x => 1 + x/2)(1.0) shouldEqual  1.99996
  }

  "calculating sqrt(4) using fixedPoint(y => 4 / y)(4)" should "be 2" in {
    Lecture2_3.sqrt(4.0) shouldEqual  2.0
  }

  "calculating sqrt(9) using fixedPoint(y => 9 / y)(9)" should "be 3" in {
    Lecture2_3.sqrt(9.0) shouldEqual  3.0
  }

    
  "calculating sqrt(2) using fixedPoint(y => 2 / y)(2)" should "be 1.41421" in {
    Lecture2_3.sqrt(2.0) shouldEqual 1.41421
  }

}
