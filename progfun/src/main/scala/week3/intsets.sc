import week3._

object intsets{
  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 1 incl 5 incl 4 incl 6 incl 0 incl 8 incl 2
  val t3 = new NonEmpty(3, Empty, Empty) incl 1 incl 5
  val t4 = new NonEmpty(2, Empty, Empty) incl 0 incl 7
  val t5 = t3 union t4

  val t:IntSet = Empty
  t match {
    case Empty => "empty"
    case null => "null"
  }

  t5.max
}