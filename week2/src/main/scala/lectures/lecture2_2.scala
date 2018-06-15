package lectures

import scala.annotation.tailrec

object Lecture2_2{
  /*
    Exercise
    1. Write a function that calculates the product of the values of a function
       for the points on a given interval
    2. write factorial in terms of product
    3. Can you write a function, which generizes both sum and product
  */

  def acumulation(op: (Int, Int) => Int, acc: Int)(f: Int => Int)(a: Int, b:Int):Int ={
    @tailrec
    def loop(a: Int, acc: Int): Int = {
      if(a > b) acc
      else loop(a + 1, op(f(a), acc))
    }
    loop(a, acc)

  }

  def sumAc = acumulation((x: Int, y: Int) => x + y, 0) _
  def mulAc = acumulation((x: Int, y: Int) => x * y, 1) _

  def factorial(n: Int):Int = mulAc((z: Int) => z)(1, n)
  def sumFac:(Int, Int) => Int = sumAc(factorial)  //<- here the type is made explicit 
  def sumCubes:(Int, Int) => Int = sumAc(x => x * x * x)
  def sumIntegers:(Int, Int) => Int = sumAc(x => x) 
  def sumSquares:(Int, Int) => Int = sumAc(x => x * x)

}  

  /*
  //same as sumTail but using currying
  def sum(f: Int => Int)(a:Int, b: Int): Int =
    if(a > b) 0 else f(a) + sum(f)( a + 1, b)
  

  def sum(f: Int => Int)(a: Int, b:Int):Int ={
    def loop(a: Int, acc: Int): Int = {
      if(a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    
    loop(a, 0)

  }
  
  //First definition  minute 1:38
  def sumTail(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = {    
      @tailrec
      def loop(a: Int, acc: Int): Int = {
        if(a > b) acc
        else loop(a + 1, f(a) + acc)
   
      }
      
      loop(a, 0) 
    
    }
    
    sumF
  }

  def factorial(n: Int): Int = {
    @tailrec
    def loop(n:Int, acc: Int): Int = {
      if(n == 0) acc
      else loop(n - 1, acc * n) 
    
    }
   
    loop(n, 1)  

  }
  
}*/
