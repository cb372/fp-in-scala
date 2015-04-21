package fpis

import org.scalatest._

class Chapter5Spec extends FlatSpec with Matchers {
  import Chapter5._

  "toList" should "return a List" in {
    Stream(1,2,3).toList should be(List(1,2,3))
  }

  "take" should "return a Stream of the first n elements" in {
    Stream(1,2,3).take(0).toList should be(Nil)
    Stream(1,2,3).take(2).toList should be(List(1,2))
    Stream(1,2,3).take(5).toList should be(List(1,2,3))
  }

  "drop" should "return a Stream without the first n elements" in {
    Stream(1,2,3).drop(0).toList should be(List(1,2,3))
    Stream(1,2,3).drop(2).toList should be(List(3))
    Stream(1,2,3).drop(5).toList should be(Nil)
  }

  "takeWhile" should "return a Stream of the starting elements that match the predicate" in {
    Stream(1,2,3).takeWhile(_ < 1).toList should be(Nil)
    Stream(1,2,3).takeWhile(_ < 2).toList should be(List(1))
    Stream(1,2,3).takeWhile(_ < 3).toList should be(List(1,2))
    Stream(1,2,3).takeWhile(_ < 4).toList should be(List(1,2,3))
  }
  
  "forAll" should "return true iff the predicate is true for all elements" in {
    Stream(1,2,3).forAll(_ % 2 == 0) should be(false)
    Stream(1,3).forAll(_ % 2 == 1) should be(false)
  }

  "takeWhile_foldRight" should "return a Stream of the starting elements that match the predicate" in {
    Stream(1,2,3).takeWhile_foldRight(_ < 1).toList should be(Nil)
    Stream(1,2,3).takeWhile_foldRight(_ < 2).toList should be(List(1))
    Stream(1,2,3).takeWhile_foldRight(_ < 3).toList should be(List(1,2))
    Stream(1,2,3).takeWhile_foldRight(_ < 4).toList should be(List(1,2,3))
  }

  "headOption_foldRight" should "return the head if there is one" in {
    Stream.empty[Int].headOption_foldRight should be(None)
    Stream(1).headOption_foldRight should be(Some(1))
  }
  

}
