object session {
  println("Welcome to Scala worksheet")
  1+2

  false || true
  true || false
  false || false
  true && true
  false && false
  false && true
  true && false

  def abs(x: Double) = if (x < 0) -x else x

  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < 0.0001
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  fixedPoint(x => 1 + x/2)(1)
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2

  def sqrt4(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)
  sqrt4(2)
  def sqrt3(x: Double) = fixedPoint(averageDamp(y => x / y))(1.0)
  sqrt3(2)

  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1 else f(a) * product(f)(a+1, b)

  def fact(n:Int): Int = product(x => x)(1, n)

  product(x => x)(1, 4)
  fact(5)
  fact(9)

  def interval(f: Int => Int, op: (Int, Int) => Int, neutral:Int = 0)(a: Int, b: Int): Int =
    if (a > b) neutral else op(f(a), interval(f, op, neutral)(a+1, b))

  def product2(a: Int, b: Int): Int = interval(x => x, (x,y) => x*y, 1)(a, b)
  def sum2(a: Int, b: Int): Int = interval(x => x, (x,y) => x+y, 0)(a, b)

  product2(1,4)
  sum2(1, 4)

  def sum2(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a+1, acc+f(a))
    }
    loop(a, 0)
  }

  def sum(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f)(a+1, b)

  sum(x => x)(1, 4)
  sum(x => x * x)(1, 4)
  sum(x => x * x * x)(1, 4)
  sum(x => x * x * x)(0, 4)
  sum(x => 2*(x*x))(0, 8)

  def balance(chars: List[Char]): Boolean = {
    def loop(n:Int, balance:Int): Int = {
      if (n == chars.length || balance < 0) return balance
      if (chars(n) == '(') return loop(n+1, balance+1)
      if (chars(n) == ')') return loop(n+1, balance-1)
      loop(n+1, balance)
    }
    if (loop(0, 0) == 0) true else false
  }
  balance("(if (zero? x) max (/ 1 x))".toList)
  balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)
  balance(":-)".toList)
  balance("())(".toList)
  balance("))((".toList)
  balance("(Hola".toList)
  balance("".toList)

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

  countChange(6, List(1,2,3))
  countChange(4, List(1,2))
  countChange(0, List(10,20))

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  sqrt(2)
  sqrt(4)
  sqrt(0.001)
  sqrt(0.1e-20)
  sqrt(1.0e20)
  sqrt(1.0e50)

  def factorial3(n: Int): Int =
    if (n == 0) 1 else n * factorial3(n - 1)

  def factorial2(n: Int, accum:Int = 1): Int = {
    if (n == 0)
      return accum
    println(accum)
    factorial2(n - 1, n * accum)
  }

  def factorial(n: Int): Int = {
    def compute(n: Int, accum:Int): Int = {
      if (n == 0) return accum
      else
        println(accum)
      compute(n - 1, accum * n)
    }
    compute(n, 1)
  }
  factorial(4)
  factorial(8)
  factorial(8-4)
  factorial(8-4) * factorial(4)

  def pascal(c: Int, r: Int): Int = {
    def factorial(n: Int): Int = {
      def compute(n: Int, accum: Int): Int =
        if (n == 0) return accum
        else compute(n - 1, accum * n)
      compute(n, 1)
    }
    factorial(r) / (factorial(r - c) * factorial(c))
  }

  pascal(4,8)
  pascal(1,3)
  pascal(0,2)
  pascal(1,2)
}
