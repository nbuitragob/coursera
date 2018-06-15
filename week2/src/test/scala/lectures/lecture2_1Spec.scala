package lectures

import org.scalatest._

class Lecture2_1Spec extends FlatSpec with Matchers {
  "The sumatory of the integers between 5 2" should "be 14" in {
    Lecture2_1.sum(x => x, 2, 5) shouldEqual 14
  }  

  "The sumatory of the square of integers between 5 3" should "be 50" in {
    Lecture2_1.sum(x => x * x, 3, 5) shouldEqual 50
  }

  "The sumatory of the cube of integers between 5 2" should "be 224" in {
    Lecture2_1.sum(x => x * x * x, 2, 5) shouldEqual 224
  }

  "The sumatory of the factorial of integers between 5 2" should "be 152" in {
    Lecture2_1.sum(Lecture2_1.factorial, 2, 5) shouldEqual 152
  }

}
