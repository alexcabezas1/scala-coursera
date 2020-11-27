package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  val s0 = singletonSet(0)
  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s1000 = singletonSet(1000)
  val s_1 = singletonSet(-1)
  val s_2 = singletonSet(-2)
  val s_3 = singletonSet(-3)
  val us12 = union(s1, s2)
  val us13 = union(s1, s3)
  val us23 = union(s2, s3)
  val us123 = union(union(s1, s2), s3)
  val us = union(union(union(union(union(s_1, s_2), s_3), s0), us123), s1000)
  printSet(us)
}
