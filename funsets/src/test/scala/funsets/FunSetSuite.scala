package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
   test("string take")                                                                                {
     val message = "hello, world"
     assert(message.take(5) == "hello")
   }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
   test("adding ints") {
     assert(1 + 2 === 3)
   }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
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
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton doesn't contain 1")
    }
  }

  test("SingletonSet(2) contains 2") {
    new TestSets {
      assert(contains(s2, 2), "Singleton doesn't contain 2")
    }
  }

  test("SingletonSet(3) contains 3") {
    new TestSets {
      assert(contains(s3, 3), "Singleton doesn't contain 3")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")

      val sb = union(us12, us13)
      assert(contains(sb, 1))
      assert(contains(sb, 2))
      assert(contains(sb, 3))
    }
  }

  test("intersect contains all elements that are both in each set"){
    new TestSets {
      var is = intersect(s1, s2)
      assert(!contains(is, 1), "Intersect 1")
      assert(!contains(is, 2), "Intersect 2")

      is = intersect(us12, s1)
      assert(contains(is, 1), "Intersect 3")
      assert(!contains(is, 2), "Intersect 4")

      is = intersect(us123, us13)
      assert(contains(is, 1), "Intersect 5")
      assert(!contains(is, 2), "Intersect 5")
      assert(contains(is, 3), "Intersect 5")
    }
  }

  test("difference contains all elements of a set that are not in the other"){
    new TestSets {
      var ds = diff(us12, s1)
      assert(contains(ds, 2), "Difference 1")
      assert(!contains(ds, 1), "Difference 2")

      ds = diff(us12, us23)
      assert(contains(ds, 1), "Difference 3")
      assert(!contains(ds, 2), "Difference 4")
      assert(!contains(ds, 3), "Difference 5")
    }
  }

  test("select elements of a set that are accepted by a given predicate"){
    new TestSets {
      var fs = filter(us, x => x > -1)
      assert(!contains(fs, -3))
      assert(!contains(fs, -2))
      assert(!contains(fs, -1))
      assert(contains(fs, 0))
      assert(contains(fs, 1))
      assert(contains(fs, 2))
      assert(contains(fs, 3))

      fs = filter(us, x => x >= -2 && x < 2)
      assert(!contains(fs, -3))
      assert(contains(fs, -2))
      assert(contains(fs, -1))
      assert(contains(fs, 0))
      assert(contains(fs, 1))
      assert(!contains(fs, 2))
      assert(!contains(fs, 3))
    }
  }

  test("wondering if all element within a set satisfy a given predicate"){
    new TestSets {
      assert(forall(us, x => x > -1000 && x < 1001))
      assert(!forall(us, x => x > 0))
      assert(!forall(us, x => x % 2 == 0))
      assert(!forall(us, x => x >= -3 && x <= 3))
      assert(!forall(us, x => x < 0))
      assert(!forall(us, x => x < 5))
    }
  }

  test("exists at least one element that satisfy a given predicate"){
    new TestSets {
      assert(exists(us, x => x < 0))
      assert(exists(us, x => x % 2 == 0))
      assert(exists(us, x => x > 5))
    }
  }

  test("transformation by applying a function to each element of a set"){
    new TestSets {
      var ts = map(us, (x) => x * 2)
      assert(contains(ts,-6))
      assert(contains(ts,-4))
      assert(contains(ts,-2))
      assert(contains(ts,0))
      assert(contains(ts,2))
      assert(contains(ts,4))
      assert(contains(ts,6))
      assert(!contains(ts,8))
    }
  }

}
