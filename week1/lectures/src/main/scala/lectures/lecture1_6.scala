package lectures

object Lecture1_6 {
  def main(args: Array[String]) =
    println("sqrt(2) = " + sqrt(2)) 
    println("sqrt(0.001) = " + sqrt(0.001)) 
    println("sqrt(0.1e-6) = " + sqrt(0.1e-6)) 
    println("sqrt(0.1e20) = " + sqrt(1.0e20)) 
    println("sqrt(1.0e50)" + sqrt(1.0e50))

  def sqrt(x: Double): Double = {
    def closeRoot(guess: Double): Double ={
      def isGoodEnough =
        abs(guess * guess - x) / x < 0.001

      def improve =
        (guess + x/guess)/2

      if(isGoodEnough) guess
      else closeRoot(improve)

    }

    def abs(y: Double) =
      if(y < 0) -y else y

    closeRoot(1.0)

  }

}

