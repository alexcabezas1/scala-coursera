object rationals {
  val x = new Rational(54, 24)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  val w = new Rational(8)
  x + y
  -x
  x - y
  x * y

  x + y * z
  x + y * z
  x - y - z
  y + y

  x < y
  x.max(y)
  x.max(z)

  class Rational(x: Int, y: Int){
    require(y != 0, "denominator must be nonzero")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    private val g = gcd(x, y)
    val n = x / g
    val d = y / g

    def < (that: Rational) =
      this.n * that.d < that.n * this.d

    def max(that: Rational) =
      if (this < that ) that else this

    def unary_- : Rational = new Rational(-this.n, this.d)

    def + (that: Rational) =
      new Rational(n * that.d + that.n * d, d * that.d)

    def - (that: Rational) = this + -that

    def * (that: Rational) =
      new Rational(n * that.n, d * that.d)

    override def toString = n + "/" + d
  }
}

