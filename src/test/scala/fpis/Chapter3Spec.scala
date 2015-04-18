package fpis

import org.scalatest._

class Chapter3Spec extends FlatSpec with Matchers {
  import Chapter3._

  "tail" should "return the tail" in {
    tail(List(1,2,3)) should be(List(2,3))
    tail(Nil) should be(Nil)
  }

  "setHead" should "return a new list with the head replaced" in {
    setHead(42, List(1,2,3)) should be(List(42,2,3))
    setHead(42, Nil) should be(List(42))
  }

  "drop" should "drop the first n elements" in {
    drop(List(1,2,3), 0) should be(List(1,2,3))
    drop(List(1,2,3), 1) should be(List(2,3))
    drop(List(1,2,3), 2) should be(List(3))
    drop(List(1,2,3), 3) should be(Nil)
    drop(List(1,2,3), 4) should be(Nil)
  }

  "dropWhile" should "drop while the predicate is true and the list is non-empty" in {
    dropWhile(List(1,2,3), {(_: Int) < 1}) should be(List(1,2,3))
    dropWhile(List(1,2,3), {(_: Int) < 2}) should be(List(2,3))
    dropWhile(List(1,2,3), {(_: Int) < 3}) should be(List(3))
    dropWhile(List(1,2,3), {(_: Int) < 4}) should be(Nil)
  }

  "init" should "return everything apart from the list item" in {
    init(List(1,2,3)) should be(List(1,2))
    init(List(1,2)) should be(List(1))
    init(List(1)) should be(Nil)
    init(Nil) should be(Nil)
  }

  "length" should "return the length" in {
    Chapter3.length(List(1,2,3)) should be(3)
    Chapter3.length(List(1,2)) should be(2)
    Chapter3.length(List(1)) should be(1)
    Chapter3.length(Nil) should be(0)
  }

  "length_foldLeft" should "return the length" in {
    length_foldLeft(List(1,2,3)) should be(3)
    length_foldLeft(List(1,2)) should be(2)
    length_foldLeft(List(1)) should be(1)
    length_foldLeft(Nil) should be(0)
  }

  "reverse_foldLeft" should "reverse the list" in {
    reverse_foldLeft(List(1,2,3)) should be(List(3,2,1))
    reverse_foldLeft(List(1,2)) should be(List(2,1))
    reverse_foldLeft(List(1)) should be(List(1))
    reverse_foldLeft(Nil) should be(Nil)
  }

  "foldRight_using_foldLeft" should "work the same as foldRight" in {
    def length_fR[A](as: List[A]): Int = foldRight_using_foldLeft(as, 0)((_, acc) => acc + 1)
    def mkString_fR[A](as: List[A]): String = foldRight_using_foldLeft(as, "")((x, acc) => s"$x $acc")
    def mkString_fL[A](as: List[A]): String = foldLeft(as, "")((acc, x) => s"$acc $x")

    length_fR(List(1,2,3)) should be(3)
    mkString_fR(List(1,2,3)) should be("1 2 3 ")
    mkString_fL(List(1,2,3)) should be(" 1 2 3")
  }

  "append" should "append" in {
    append(List(1,2,3), 4) should be(List(1,2,3,4))
    append(List(1,2), 4) should be(List(1,2,4))
    append(List(1), 4) should be(List(1,4))
    append(Nil, 4) should be(List(4))
  }

  "flatten" should "flatten" in {
    flatten(List(List(1,2),List(3,4))) should be(List(1,2,3,4))
  }
  
  "mapAddOne" should "add one to each element" in {
    mapAddOne(List(1,2,3)) should be(List(2,3,4))
  }
  
  "mapToString" should "convert each element to a string" in {
    mapToString(List(1.0,2.0,3.0)) should be(List("1.0","2.0","3.0"))
  }

  "removeOdd" should "remove odd numbers" in {
    removeOdd(List(1,2,5,4,3)) should be(List(2,4))
  }

  "flatMap" should "map and flatten" in {
    flatMap(List(1,2,3))(i => List(i,i)) should be(List(1,1,2,2,3,3))
  }

  "filter_flatMap" should "work the same as filter" in {
    def removeOdd_flatMap(as: List[Int]) = filter_flatMap(as)(_ % 2 == 0)
    removeOdd_flatMap(List(1,2,5,4,3)) should be(List(2,4))
  }

  "zipAdd" should "add two lists" in {
    zipAdd(List(1,2,3), List(4,5,6)) should be(List(5,7,9))
  }

  "zipWith" should "add two lists" in {
    zipWith(List(1,2,3), List(4,5,6))(_ + _) should be(List(5,7,9))
  }

  "hasSubsequence" should "return whether the list contains the subsequence" in {
    hasSubsequence(List(1,2,3,4), List(1,2)) should be(true)
    hasSubsequence(List(1,2,3,4), List(2,3)) should be(true)
    hasSubsequence(List(1,2,3,4), List(4)) should be(true)
    hasSubsequence(List(1,2,3,4), List(5)) should be(false)
    hasSubsequence(List(1,2,3,4), List(3,2)) should be(false)
  }
  
  "size" should "return the size of the tree" in {
    Chapter3.size(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))) should be(5)
  }
  
  "maximum" should "return the maximum value in the tree" in {
    maximum(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))) should be(3)
  }
  
  "depth" should "return the maximum depth of the tree" in {
    depth(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))) should be(3)
  }
  
  "map" should "apply the function to every leaf" in {
    map(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3)))(_ + 1) should be(Branch(Branch(Leaf(2),Leaf(3)),Leaf(4)))
  }

  "size_fold" should "return the size of the tree" in {
    size_fold(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))) should be(5)
  }
  
  "maximum_fold" should "return the maximum value in the tree" in {
    maximum_fold(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))) should be(3)
  }
  
  "depth_fold" should "return the maximum depth of the tree" in {
    depth_fold(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))) should be(3)
  }
  
  "map_fold" should "apply the function to every leaf" in {
    map_fold(Branch(Branch(Leaf(1),Leaf(2)),Leaf(3)))(_ + 1) should be(Branch(Branch(Leaf(2),Leaf(3)),Leaf(4)))
  }
  

}
