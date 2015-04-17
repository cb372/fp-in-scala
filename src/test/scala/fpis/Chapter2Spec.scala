package fpis

import org.scalatest._

class Chapter2Spec extends FlatSpec with Matchers {
  import Chapter2._

  "isSorted" should "check an array is sorted" in {
    val ordered = (a1: Int, a2: Int) => a1 <= a2
    isSorted(Array(1,2,3), ordered) should be (true)
    isSorted(Array(1,3,2), ordered) should be (false)
  }

  "curry" should "curry" in {
    val f = { (a: Int, b: Int) => a * b }
    val curried = curry(f)
    val partial = curried(2)
    partial(3) should be(6)
  }

  "uncurry" should "uncurry" in {
    val f = { (a: Int) => (b: Int) => a * b }
    val uncurried = uncurry(f)
    uncurried(2, 3) should be(6)
  }

  "compose" should "compose" in {
    val f = { (a: Int) => a * 2 }
    val g = { (a: Int) => a + 1 }
    compose(f, g)(3) should be(8)
  }
}
