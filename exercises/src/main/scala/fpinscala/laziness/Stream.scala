package fpinscala.laziness

import Stream._
trait Stream[+A] {

  // Note: kae:
  //  - z is what you get on an empty list
  //  - B is the type of the result of the function
  def foldRight[B](z: => B)
    (f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise 5.1
  def toList: List[A] = this match {
    case Empty ⇒ List.empty
    case Cons(h, t) ⇒ h() :: t().toList
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Empty ⇒ Empty

    case Cons(_, _) if n <= 0 ⇒ Empty

    case Cons(h, t) ⇒ Cons(h, () ⇒ t().take(n - 1))
  }

  // Exercise 5.2
  def drop(n: Int): Stream[A] = this match {
    case Empty ⇒ Empty
    case Cons(_, _) if n <= 0 ⇒ this
    case Cons(_, t) ⇒ t().drop(n - 1)
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty ⇒ Empty
    case Cons(h, _) if !p(h()) ⇒ Empty
    case Cons(h, t) ⇒ Cons(h, () ⇒ t().takeWhile(p))
  }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) ⇒ p(a) && b }

  // Exercise 5.5
  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A]  = {
    foldRight(empty[A]) { (a, as) ⇒
      if (!p(a)) {
        empty[A]
      } else {
        cons(a, as.takeWhile(p))
      }
    }
  }

  // Exercise 5.6
  def headOption: Option[A] = {
    // Use foldRight
    foldRight(Option.empty[A]) { (a, _) => Some(a)   }
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A ⇒ B): Stream[B] = {
    foldRight(empty[B]) { (a, bs) ⇒
      cons(f(a), bs)
    }
  }

  def filter(p: A ⇒ Boolean): Stream[A] = {
    foldRight(empty[A]) { (a, as) ⇒
      if (p(a)) {
        cons(a, as)
      } else {
        as
      }
    }
  }

  def append[B >: A](bs: ⇒ Stream[B]) : Stream[B] = {
    foldRight(bs) { (bb, bbs) ⇒
      cons(bb, bbs)
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    (this, s) match {
      case (_, Empty) ⇒ true
      case (Empty, _) ⇒ false
      case (Cons(h1, t1), Cons(h2, t2)) ⇒
        h1() == h2() && t1().startsWith(t2())
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
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 5.8
  def constant[A](a: A): Stream[A]  = cons(a, constant(a))

  // Exercise 5.9
  def from(n: Int): Stream[Int] = cons(n, Stream.from(n + 1))

  // Exercise 5.10
  def fibs: Stream[Int] = {
    def f(a: Int, b: Int): Stream[Int] = {
      val next = a + b
      cons(next, f(b, a + b))
    }

    cons(0, cons(1, f(0, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}