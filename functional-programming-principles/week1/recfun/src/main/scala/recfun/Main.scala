package recursion

object Main {
  def main(args: Array[String])={}

  def pascal(c: Int, r: Int): Int = {
    def factorial(n: Int)={
      def factorialIterator(acc: Int, n: Int): Int ={
        if(n == 0) acc else factorialIterator(acc * n, n - 1)
      }

      factorialIterator(1, n)

    }
    
    factorial(r) / (factorial(c) * factorial(r-c))

  }
  
  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.toIndexedSeq.sorted
    val size = sortedCoins.length
    def countChange(currentMoney: Int, count: Int, head: Int, i: Int): Int = {
      if(head + 1 >= size) count
      else if(i == 0) countChange(money % (head + 1), count, head + 1, (money - money % sortedCoins(head + 1))/sortedCoins(head + 1))
      else
          if(currentMoney == 0) 
            countChange(money - sortedCoins(head)*(i-1), count + 1, head, i-1)

          else if(currentMoney%sortedCoins(head + 1) == currentMoney)    
            countChange(money - sortedCoins(head)*(i-1), count, head, i-1)

          else if(currentMoney%sortedCoins(head + 1) == 0)   
            countChange(money - sortedCoins(head)*(i-1), count + 1, head, i-1)

          else countChange(currentMoney%(head + 1), count, head, i)
         
    }

    if(money == 0 || size == 0) 0
    else countChange(money % sortedCoins(0), 0, 0, (money - money % sortedCoins(0))/sortedCoins(0))
  } 

  def balance(chars: List[Char]): Boolean = {
    def isBalanced(phrase: List[Char], isClosing: Boolean, nOpeningParens: Int): Boolean = {
        phrase match{
          case Nil => isClosing && nOpeningParens == 0
          case x :: xs => {
            if((x == ')' && nOpeningParens == 0) || nOpeningParens < 0) false
            else if(x == '(') isBalanced(xs, false, nOpeningParens + 1)
            else if(x == ')') isBalanced(xs, true, nOpeningParens - 1)
            else isBalanced(xs, false, nOpeningParens)
        }
      }
    
    }

    isBalanced(chars, false, 0)
  }
  
}
