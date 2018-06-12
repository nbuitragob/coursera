package recursion

object Main {
  def main(args: Array[String])={}

  def pascal(c: Int, r: Int) ={
    def factorial(n: Int)={
      def factorialIterator(acc: Int, n: Int): Int ={
        if(n == 0) acc else factorialIterator(acc * n, n - 1)
      }

      factorialIterator(1, n)

    }
    
    factorial(r) / (factorial(c) * factorial(r-c))

  }

  def getParens(phrase: List[Char]) ={
    def selectParens(current_phrase: List[Char], ans: List[Char]): List[Char] ={
      if(current_phrase.isEmpty) ans 
      else if(isParens(current_phrase.head)) selectParens(current_phrase.tail, ans ::: List(current_phrase.head))
      else selectParens(current_phrase.tail, ans) 
    }

    def isParens(x: Char)=
      x == '(' || x == ')'

    selectParens(phrase, Nil).mkString
  
  }
  
}
