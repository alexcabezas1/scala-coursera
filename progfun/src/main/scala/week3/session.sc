import week3._
object using_list {
  def singleton[T](elem: T): List[T] = new Cons[T](elem, new Nil[T])
  def nth[T](n: Int, xs: List[T]): T = {
    println(n + " " + xs)
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) xs.head
    else nth(n - 1, xs.tail)
  }

  val l1 = singleton(1)
  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
  nth(2, list)
  //nth(-1, list)
  //nth(5, list)
}