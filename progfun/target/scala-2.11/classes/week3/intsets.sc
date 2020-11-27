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

  abstract class IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
    def max: Int
  }

  object Empty extends IntSet {
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
    def union(other: IntSet): IntSet = other
    def max: Int = 0
    override def toString = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet{
    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    def union(other: IntSet): IntSet = {
      //println(left + " u " + right + " u " + other + " << " + elem)
      ((left union right) union other) incl elem
    }

    def max: Int = {
      def compare(x: Int, y: Int): Int = if (x > y) x else y
      compare(compare(elem, left.max), right.max)
    }
    override def toString = "{" + left + elem + right + "}"
  }
}