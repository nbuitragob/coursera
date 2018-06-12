package lectures

object Lecture1_7{
  def main(args: Array[String])=
    println(factorial(9))
    println(gcd(14, 21))

  def gcd(a: Int, b: Int): Int =
    if(b == 0) a else gcd(b, a%b)

  def factorial(n: Int) = {
    def factorialIter(n: Int, acc: Int): Int =
      if(n == 0) acc else factorialIter(n - 1, acc * n)

    factorialIter(n, 1)
  }
}
