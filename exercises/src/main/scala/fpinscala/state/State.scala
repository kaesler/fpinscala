package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 6.1
  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, g) = rng.nextInt
    if (i == Int.MinValue) {
      // Skip this one
      nonNegativeInt(g)
    } else {
      (i, g)
    }
  }

  // Exercise 6.2
  @tailrec
  def double(rng: RNG): (Double, RNG) = {
    val (i, g) = nonNegativeInt(rng)
    if (i == Int.MaxValue) {
      // Skip this one
      double(g)
    } else {
      (i.toDouble / Int.MaxValue.toDouble, g)
    }
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, g1) = rng.nextInt
    val (d, g2) = double(g1)
    ((i, d), g2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, g1) = double(rng)
    val (d2, g2) = double(g1)
    val (d3, g3) = double(g2)
    ((d1, d2, d3), g3)
  }


  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    // Make a dummy list of right size and foldLeft.
    List.fill(count)(())
      .foldLeft[(List[Int], RNG)]((Nil, rng)) { case ((is, lastRng), _) =>
      val (nextI, nextRng) = lastRng.nextInt
      (is :+ nextI, nextRng)
    }
  }

  // Exercise 6.5
  def doubleUsingMap(rng: RNG): (Double, RNG) = {
    // Produces random Ints that can be converted to Double
    val nonMinimumInt: Rand[Int] = { rng =>
      var res = rng.nextInt
      while(res._1 == Int.MinValue) {
        res = rng.nextInt
      }
      res
    }
    map(nonMinimumInt)(_.toDouble)(rng)
  }

  // Exercise 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    val res: Rand[C] = { rng =>
      val (a, g1) = ra(rng)
      val (b, g2) = rb(g1)
      (f(a,b), g2)
    }
    res
  }

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
    val res: (List[A], RNG) = {
      fs
        .foldLeft[(List[A], RNG)]((Nil, rng)) { case ((as,rng), lastRand) =>
        val (nextA, nextRng) = lastRand(rng)
        (as :+ nextA, nextRng)
      }
    }
    res
  }
  def intsUsingSequence(count: Int)(rng: RNG): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  // Exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val res: (B, RNG) = {
      // Need to get an A so we can use g
      val (a, g1) = f(rng)
      val randB: Rand[B] = g(a)
      randB(g1)
    }
    res
  }
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) {
        unit(mod)
      } else {
        nonNegativeLessThan(n)
      }
    }
  }
}

case class State[S,+A](run: S => (A, S)) {

  // Exercise 6.9
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State[S, B](
      run = { s =>
        // First we need an A
        val (a, s1) = run(s)
        // Now we can apply f to the A ...
        f(a).run(s1)
      }
    )
  }

  def map[B](f: A => B): State[S, B] = {
    flatMap { a =>
      State[S, B](
        run = { s: S =>
          (f(a), s)
        }
      )
    }
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap { a =>
      State[S, C](
        run = { s =>
          val (b, s1) = sb.run(s)
          (f(a, b), s1)
        }
      )
    }
  }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
