package lectures

import scala.annotation.tailrec

object Lecture2_1{
  def main(args: Array[String])= {}

  def sum(f: Int => Int, a: Int, b: Int): Int = {
      @tailrec
      def loop(a: Int, acc: Int): Int = {
        if(a > b) acc
        else loop(a + 1, f(a) + acc)
   
      }
      
      loop(a, 0) 
  
  }
  

  def factorial(n: Int): Int = {
    @tailrec
    def loop(n:Int, acc: Int): Int = {
      if(n == 0) acc
      else loop(n - 1, acc * n) 
    
    }
   
    loop(n, 1)  

  }
    
}
