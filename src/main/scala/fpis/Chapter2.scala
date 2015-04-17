package fpis

import scala.annotation.tailrec

object Chapter2 {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else ordered(as(n), as(n + 1)) && loop(n + 1)
    }
    loop(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = { a => b => f(a, b) }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = { (a, b) => f(a)(b) }

  def compose[A,B,C](f: B => C, g: A => B): A => C = { x => f(g(x)) }
}
