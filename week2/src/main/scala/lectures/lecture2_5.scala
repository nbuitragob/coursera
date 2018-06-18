package lectures


object Lecture2_5{
  def main(args: Array[String]) = {
    var g = new Rational(1,3)
    val h = new Rational(5,7)
    val z = new Rational(3,2)
    println("G = " + g.toString)
    println("H = " + h.toString)
    println(g.substract(h).substract(z))

  }  
}

class Rational(var x: Int, var y: Int){
  def numerator = x
  def denominator = y

  def add(z: Rational): Rational  = {
    x = numerator * z.denominator + z.numerator * denominator
    y = denominator * z.denominator
    new Rational(x, y)
    /*val gcd1 = gcd
    x = x / gcd1
    y = y / gcd1
    checkSign*/
  }

  def neg: Rational = 
    new Rational(-x, y)

  def inverse =
    if(x == 0) "NaN"
    else new Rational(y, x)


  def multiply(z: Rational): Rational = {
    x = x * z.denominator
    y = y * z.numerator
    //checkSign
    new Rational(x, y)
  }

  private def gcd: Int = {
    def iterator(num: Int, dem: Int): Int = {
      if(dem == 0) num 
      else iterator(dem, num % dem) 
    
    }

    iterator(x, y)

  }

  def substract(z: Rational): Rational = {
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




