import week4._
import week3._

object using_nat {
  val nat0 = Zero
  val nat1 = new Succ(Zero)
  val nat2 = new Succ(nat1)
  val nat3 = new Succ(nat2)
  nat3.number
  println(nat0)
  println(nat1)
  println(nat2)
  println(nat3)
  println(nat2 + nat2)
  println(nat3 + nat2)

  List()
  List(1)
  List(1, 2)
  List(1, 2, 3)
  List("a")
  List("a", "b")
  List("a", "b", "c")
  List("a", "b", "c", "d")
  /*
  val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))
  val b: Array[IntSet] = a
  b(0) = Empty
  val s: NonEmpty = a(0)

  def f(xs: List[NonEmpty], x: Empty) = xs prepend x
  */

  Sum(Number(10), Number(2)).eval
  Sum(Number(10), Number(2)).show
  Expr.show(Sum(Number(10), Number(2)))
  Expr.show(Sum(Prod(Number(2), Var("x")), Var("y")))

  def show(e: Expr): String = e match {
    case Sum(l:Prod, r) => show(l.e1) + " * (" + show(l.e2) + " + " + show(r) + ")"
    case Sum(l, r) => show(l) + " + " + show(r)
    case Prod(l:Sum, r) => "(" + show(l) + ") * " + show(r)
    case Prod(l, r) => show(l) + " * " + show(r)
    case Var(x) => x.toString
    case x: Number => x.eval.toString
  }

  show(Sum(Prod(Number(2), Var("x")), Var("y")))
  show(Prod(Sum(Number(2), Var("x")), Var("y")))

  def isort(xs: scala.List[Int]): scala.List[Int] = xs match {
    case scala.List() => scala.List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: scala.List[Int]): scala.List[Int] = xs match {
    case scala.List() => scala.List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }

  isort(scala.List(3,7,4,9,5,2,6,1))

  2 :: 1 :: scala.List(7,3,9)
  scala.List(1) :: scala.List(2,3)
  scala.List(1).head :: 1 :: scala.collection.immutable.Nil
  scala.List(1).head :: scala.List(2).head  :: scala.collection.immutable.Nil
  scala.List(1,2,3) ::: scala.List(5,6)

  val l = scala.List(1, 2, 4, 2, 4, 7, 3, 2, 4)
  l.groupBy(x => x).map { case (k,v) => (k, v.length) }.toList

  (1, Nil) :: scala.List()
  (1, Nil) :: (1, Nil) :: scala.List()
  val l1 = 3 :: 1 :: scala.List()
  2 :: l1.tail ::: scala.List(1)
  scala.List(1,2) ++ scala.List(3,4)
  scala.List(1,2) ::: scala.List(3,4)
}