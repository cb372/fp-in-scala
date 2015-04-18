package fpis

import scala.annotation.tailrec

object Chapter3 {

  // copied from the book
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }
    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }
    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }
  }

  def tail[A](list: List[A]) = list match {
    case Cons(x, xs) => xs
    case Nil => Nil
  }

  def setHead[A](newHead: A, list: List[A]) = Cons(newHead, tail(list))

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(a, as) =>
      if (f(a)) dropWhile(tail(l), f)
      else l
    case Nil => Nil
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(a, Nil) => Nil
    case Cons(a, as) => Cons(a, init(as))
  }

  // copied from the book
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(l: List[A], acc: B): B = l match {
      case Nil => acc
      case Cons(x, xs) => loop(xs, f(acc, x))
    }
    loop(as, z)
  }

  def sum_foldLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  def product_foldLeft(as: List[Int]): Int = foldLeft(as, 1)(_ * _)
  def length_foldLeft[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)
  
  def reverse_foldLeft[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, x) => Cons(x, acc))

  def foldRight_using_foldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, identity[B] _)((g, a) => b => g(f(a, b)))(z)
  }

  def append[A](as: List[A], a: A): List[A] = {
    foldRight(as, List(a))((x, acc) => Cons(x, acc))
  }

  def concat[A](as1: List[A], as2: List[A]): List[A] = {
    foldRight(as1, as2)((x, acc) => Cons(x, acc))
  }

  def flatten[A](xss: List[List[A]]): List[A] = {
    foldRight(xss, Nil: List[A])((xs, acc) => foldRight(xs, acc)((x, acc2) => Cons(x, acc2)))
  }

  def mapAddOne(xs: List[Int]): List[Int] = {
    foldRight(xs, Nil: List[Int])((x, acc) => (Cons(x + 1, acc)))
  }

  def mapToString(xs: List[Double]): List[String] = {
    foldRight(xs, Nil: List[String])((x, acc) => (Cons(x.toString, acc)))
  }
  
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((x, acc) => (Cons(f(x), acc)))
  }
  
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((x, acc) => (if (f(x)) Cons(x, acc) else acc))
  }

  def removeOdd(as: List[Int]) = filter(as)(_ % 2 == 0)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B])((x, acc) => concat(f(x), acc))
  }

  def filter_flatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap[A,A](as)(x => (if (f(x)) List(x) else Nil))
  }

  def zipAdd(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, Nil) => Nil
    case (Nil, Cons(y, ys)) => Cons(y, zipAdd(Nil, ys))
    case (Cons(x, xs), Nil) => Cons(x, zipAdd(xs, Nil))
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipAdd(xs, ys))
  }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, Nil) => Nil
    case (Nil, Cons(_, _)) => Nil
    case (Cons(_, _), Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith(ls: List[A], sub: List[A]): Boolean = (ls, sub) match {
      case (Nil, Nil) => true
      case (Cons(_, _), Nil) => true
      case (Nil, Cons(_, _)) => false
      case (Cons(a, as), Cons(b, bs)) => a == b && startsWith(as, bs)
    }

    (sup, sub) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (Cons(a, as), _) => startsWith(sup, sub) || hasSubsequence(as, sub)
    }
  }

  // copied from book
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](tree: Tree[A])(f: A => B, g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f, g), fold(r)(f, g))
  }

  def size_fold[A](tree: Tree[A]): Int = {
    fold[A, Int](tree)((_ => 1), ((l, r) => 1 + l + r))
  }

  def maximum_fold(tree: Tree[Int]): Int = {
    fold[Int, Int](tree)((v => v), ((l, r) => l max r))
  }

  def depth_fold[A](tree: Tree[A]): Int = {
    fold[A, Int](tree)((_ => 1), ((l, r) => 1 + (l max r)))
  }

  def map_fold[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold[A, Tree[B]](tree)((v => Leaf(f(v))), ((l, r) => Branch(l, r)))
  }

}
