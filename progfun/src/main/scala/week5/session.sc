object session {
  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case Nil => throw new Error("init of empty list")
    case x :: Nil => Nil
    case y :: ys => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => List()
    case y :: ys => reverse(ys) ++ List(y)
  }

  def removeAt2[T](n: Int, xs: List[T]): List[T] = {
    def remove(n: Int, ys: List[T], acc: List[T]): List[T] = ys match {
      case Nil => acc
      case z :: zs => if (xs.indexOf(z) == n) remove(n, zs, acc) else remove(n, zs, acc ++ List(z))
    }
    if (0 > n || n > (xs.length-1)) xs
    else remove(n, xs, Nil)
  }

  def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n+1)
  val l = List(1,2,10,20)
  last(l)
  init(l)
  init(List(1,2,3))

  concat(l, List(1,2,3))
  reverse(l)
  l.indexOf(2)
  (l take 2) ++ (l drop 3)
  removeAt(3, l)
  removeAt(1, List('a','b','c','d'))
  removeAt(-1, l)
  removeAt(5, l)

  def flatten[T](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case (y:List[T]) :: ys => flatten(y) ::: flatten(ys)
    case (y:T) :: ys => y :: flatten(ys)
  }

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))

  def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length/2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (lt(x,y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst)(lt), msort(snd)(lt))
    }
  }
  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")
  msort(nums)((x: Int, y: Int) => x < y)
  msort(fruits)((x: String, y: String) => x.compareTo(y) < 0)

  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => (y*y) :: squareList(ys)
  }

  def squareList2(xs: List[Int]): List[Int] =
    xs map { x => x*x }

  squareList(List(1,2,3,4,-2))
  squareList2(List(1,2,3,4,-2))
}