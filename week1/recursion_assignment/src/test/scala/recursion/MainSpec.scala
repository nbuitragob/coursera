package recursion

import org.scalatest._

class MainSpec extends FlatSpec with Matchers {
  "Main.pascal(0,2)" should "return 1" in {
    Main.pascal(0,2) shouldEqual 1 
  }

  "Main.pascal(1,2)" should "return 2" in {
    Main.pascal(1,2) shouldEqual 2
  }

  "Main.pascal(1,3)" should "return 3" in {
    Main.pascal(1,3) shouldEqual 3
  }

  ("Main.getParens((if (zero? x) max (/ 1 x)))" 
        should "return (()())") in {
    
    (Main.getParens("(if (zero? x) max (/ 1 x))".toList) 
        shouldEqual "(()())")
  } 
}

