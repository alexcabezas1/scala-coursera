object listfun {
  val nums = List(2, 8, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")

  nums filter (x => x > 0)
  nums filterNot {x => x > 0}
  nums partition (x => x > 0)
  nums takeWhile (x => x > 0)
  nums dropWhile (x => x > 0)
  nums span (x => x > 0)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case y :: ys =>
      val (first, rest) = y :: ys span (x => x == y)
      first :: pack(rest)
  }
  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map { ys => (ys.head, ys.length) }

  val data = List('a','a','a','b','c','c','a')
  pack(data)
  encode(data)
}