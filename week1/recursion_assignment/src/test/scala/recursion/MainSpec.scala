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

  ("Main.balance('(if (zero? x) max (/ 1 x))')" 
        should "return true") in {
    
    (Main.balance("(if (zero? x) max (/ 1 x))".toList) 
        shouldEqual true)
  }

  "Main.countChange(4, List(1,2))" should "return 3" in {
    Main.countChange(4, List(1,2)) shouldEqual 3
  }

  "Main.countChange(4, List(1,2,3))" should "return 3" in {
    Main.countChange(4, List(1,2,3)) shouldEqual 4
  }

  "Main.countChange(222, List(1,5,10,20,50,100)" should "return 3" in {
    Main.countChange(222, List(1,5,10,20,50,100)) shouldEqual 3
  }

  "Main.countChange(300,List(500,5,50,100,20,200,10))" should "return 1022" in {
    Main.countChange(300,List(500,5,50,100,20,200,10)) shouldEqual 1022 
  }

  "Main.countChange(100,List(1,10,5,10,25,50))" should "return 292" in {
    Main.countChange(100,List(1,10,5,10,25,50)) shouldEqual 292 
  }

}

