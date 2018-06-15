package lectures


object Lecture2_5{
  def main(args: Array[String]) = {
    var g = new Rational(1,2)
    val h = new Rational(5,6)
    val z = new Rational(3,4)
    println("G = " + g.toString)
    println("H = " + h.toString)
    g.substract(h)
    g.substract(z)
    println("G + H = " + g.toString)

  }  
}

class Rational(var x: Int, var y: Int){
  def numerator = x
  def denominator = y

  def add(z: Rational): Unit  = {
    x = numerator * z.denominator + z.numerator * denominator
    y = denominator * z.denominator
    val gcd1 = gcd
    x = x / gcd1
    y = y / gcd1
    checkSign
  }

  def neg: Rational = 
    new Rational(-x, y)

  def inverse =
    if(x == 0) "NaN"
    else new Rational(y, x)


  def multiply(z: Rational): Unit = {
    x = x * z.denominator
    y = y * z.numerator
    checkSign
  }

  def gcd: Int = {
    def iterator(num: Int, dem: Int): Int = {
      if(dem == 0) num 
      else iterator(dem, num % dem) 
    
    }

    iterator(x, y)

  }

  def substract(z: Rational): Unit = {
    add(z.neg)
  }

  override def toString: String =
    numerator + " / " + denominator
  
  def checkSign =
    if(y < 0) { 
      x = -x 
      y = -y 
    }
  
}




