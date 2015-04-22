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

  "map_foldRight" should "map the function" in {
    Stream(1,2,3).map_foldRight(_.toString).toList should be(List("1","2","3"))
  }

  "filter_foldRight" should "filter the stream" in {
    Stream(1,2,3).filter_foldRight(_ % 2 == 1).toList should be(List(1,3))
  }

  "append_foldRight" should "concatenate the streams" in {
    Stream(1,2,3).append_foldRight(Stream(4,5)).toList should be(List(1,2,3,4,5))
  }

  "flatMap_foldRight" should "flatMap the function" in {
    Stream(1,2,3).flatMap_foldRight(_ => Stream("a","b")).toList should be(List("a","b","a","b","a","b"))
  }
  
  "constant" should "return an infinite stream of constants" in {
    Stream.constant(42).take(3).toList should be(List(42,42,42))
  }
  
  "from" should "return an infinite stream of increasing numbers" in {
    Stream.from(42).take(3).toList should be(List(42,43,44))
  }
  
  "fibs" should "return an infinite stream of Fibonacci numbers" in {
    Stream.fibs.take(8).toList should be(List(0,1,1,2,3,5,8,13))
  }
  
  "unfold" should "return a stream defined by a function" in {
    Stream.unfold(0)(s => Some(s, s+1)).take(4).toList should be(List(0,1,2,3))
  }
  
  "fibs_unfold" should "return an infinite stream of Fibonacci numbers" in {
    Stream.fibs_unfold.take(8).toList should be(List(0,1,1,2,3,5,8,13))
  }
  
  "from_unfold" should "return an infinite stream of increasing numbers" in {
    Stream.from_unfold(42).take(3).toList should be(List(42,43,44))
  }

  "constant_unfold" should "return an infinite stream of constants" in {
    Stream.constant_unfold(42).take(3).toList should be(List(42,42,42))
  }

  "map_unfold" should "map the function" in {
    Stream(1,2,3).map_unfold(_.toString).toList should be(List("1","2","3"))
  }
  
  "take_unfold" should "return a Stream of the first n elements" in {
    Stream(1,2,3).take_unfold(0).toList should be(Nil)
    Stream(1,2,3).take_unfold(2).toList should be(List(1,2))
    Stream(1,2,3).take_unfold(5).toList should be(List(1,2,3))
  }


}
