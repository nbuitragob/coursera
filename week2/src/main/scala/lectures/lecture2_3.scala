package lectures

import scala.math.{abs, pow}


object Lecture2_3{
  val tolerance = 0.00001
  def fixedPoint(improve: Double => Double)(guess: Double): Double = {
    def isGoodEnough(x: Double, y: Double): Boolean = 
      abs((x - y) / x) < tolerance
    
    def iterator(guess: Double): Double = {
      val next = improve(guess)
      if (isGoodEnough(guess, next)) guess
      else iterator(next)
    }
  
    setPrecision(iterator(1.0), 5)
  }
  
  def setPrecision(value: Double, precision: Int): Double ={
    (value*Math.pow(10,precision)).toInt/Math.pow(10,precision)
  }

  //calculing square root using fixedPoint and 
  //since sqrt(x) is a fixed point of f(y) = x / y where x is positive real number. 
  //Though this doen't (because it never converges) work so Average Damping is being used  
  
  def averageDamp(f: Double => Double)(x: Double): Double = {
    (x + f(x))/2
  }

  def sqrt(x: Double): Double = {
    fixedPoint(averageDamp(z => x/z))(x)
  }

}
