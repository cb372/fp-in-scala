package fpis

import scala.annotation.tailrec

object Chapter5 {

  sealed trait Stream[+A] {
    import Stream._

    def headOption: Option[A] = this match { 
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = {
      if (n <= 0) empty
      else this match {
        case Empty => this
        case Cons(h, t) => cons(h(), t().take(n-1))
      }
    }

    def drop(n: Int): Stream[A] = {
      if (n <= 0) this
      else this match {
        case Empty => this
        case Cons(h, t) => t().drop(n-1)
      }
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Empty => this
      case Cons(h, t) =>
        if (p(h())) cons(h(), t().takeWhile(p))
        else empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def forAll(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) && b)

    def takeWhile_foldRight(p: A => Boolean): Stream[A] =
      foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else empty)

    def headOption_foldRight: Option[A] =
      foldRight[Option[A]](None)((h, t) => Some(h))
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }


}
