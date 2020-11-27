package funsets

import scala.annotation.tailrec


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
    def singletonSet(elem: Int): Set = (x) => x == elem
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
    def union(s: Set, t: Set): Set = (x) => s(x) || t(x)
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = (x) => s(x) && t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = (x) => s(x) && !t(x)
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = (x) => s(x) && p(x)
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean, atleastone: Boolean = false): Boolean = {
      def iter(a: Int, found:Int = 0, positives:Int = 0): Boolean = {
        if (atleastone && positives > 0) return true
        if (a > bound) positives == found
        else if (contains(s, a)) {
          if (p(a)) iter(a+1, found+1, positives+1)
          else iter(a+1, found+1, positives)
        }
        else iter(a+1, found, positives)
      }
      iter(-bound)
    }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = forall(s, p, true)
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = {
      def iter(a: Int, acc: Set): Set = {
        if (a == bound){
          if (contains(s, a)) union(acc, singletonSet(f(a)))
          else acc
        }
        else {
          if (contains(s, a)) iter(a+1, union(acc, singletonSet(f(a))))
          else iter(a+1, acc)
        }
      }
      iter(-bound, singletonSet(-9999))
    }
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
