package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      def factorial(n: Int): Int = {
        def compute(n: Int, accum: Int): Int =
          if (n == 0) return accum
          else compute(n - 1, accum * n)
        compute(n, 1)
      }
      factorial(r) / (factorial(r - c) * factorial(c))
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(n:Int, balance:Int): Int = {
        if (n == chars.length || balance < 0) return balance
        if (chars(n) == '(') return loop(n+1, balance+1)
        if (chars(n) == ')') return loop(n+1, balance-1)
        loop(n+1, balance)
      }
      if (loop(0, 0) == 0) true else false
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def calculate(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if (money < 0 || coins.isEmpty) 0
        else {
          calculate(money - coins.head, coins) +
            calculate(money, coins.tail)
        }
      }
      if (money == 0) 0
      else calculate(money, coins)
    }
}
