package lectures

object Lecture1_5 {
  def main(args: Array[String]) =
    println("sqrt(2) = " + sqrt(2)) 
    println("sqrt(0.001) = " + sqrt(0.001)) 
    println("sqrt(0.1e-6) = " + sqrt(0.1e-6)) 
    println("sqrt(0.1e20) = " + sqrt(1.0e20)) 
    println("sqrt(1.0e50)" + sqrt(1.0e50))

  def sqrt(x: Double): Double =
    closeRoot(x, 1.0)

  def closeRoot(x: Double, guess: Double): Double =
    if(isGoodEnough(x,guess)) guess
    else closeRoot(x, improve(x,guess))

  def isGoodEnough(x: Double, guess: Double) =
    abs(guess * guess - x) / x < 0.001

  def improve(x: Double, guess: Double) =
    (guess + x/guess)/2

  def abs(x: Double) =
    if(x < 0) -x else x
}

