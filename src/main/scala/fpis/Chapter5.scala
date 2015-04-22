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

    def map_foldRight[B](f: A => B): Stream[B] =
      foldRight[Stream[B]](empty)((a, b) => cons(f(a), b))

    def filter_foldRight(p: A => Boolean): Stream[A] =
      foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else b)

    def append_foldRight[B >: A](other: => Stream[B]): Stream[B] =
      foldRight[Stream[B]](other)((a, b) => cons(a, b))

    def flatMap_foldRight[B](f: A => Stream[B]): Stream[B] =
      foldRight[Stream[B]](empty)((a, b) => f(a).append_foldRight(b))

    def map_unfold[B](f: A => B): Stream[B] =
      unfold(this) {
        case Cons(h,t) => Some((f(h()), t()))
        case _ => None
      }

    def take_unfold[B](n: Int): Stream[A] =
      unfold(n, this) { case (x, s) =>
        s match {
          case Cons(h,t) => 
            if (x < 1) None
            else if (x == 1) Some((h(), (0, empty)))
            else Some((h(), (n-1, t())))
          case _ => None
        }
      }

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

    def constant[A](a: A): Stream[A] = {
      lazy val tail: Stream[A] = Cons(() => a, () => tail) 
      tail
    }

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def fibs: Stream[Int] = {
      def loop(i: Int, j: Int): Stream[Int] =
        cons(i, loop(j, i + j))
      loop(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

    def fibs_unfold: Stream[Int] =
      unfold[Int, (Int, Int)]((0, 1)){ case (i, j) => Some((i, (j, i+j))) }

    def from_unfold(n: Int): Stream[Int] =
      unfold(n)(x => Some((x, x + 1)))

    def constant_unfold[A](a: A): Stream[A] =
      unfold(a)(x => Some(x, x))

    def ones_unfold[A]: Stream[Int] =
      unfold(1)(x => Some(x, x))


  }


}
