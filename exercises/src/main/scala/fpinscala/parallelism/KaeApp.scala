package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Future}

object KaeApp {

  trait Par[A] {

    // Exercise 7.1.
    def map2(p1: => Par[A], p2: => Par[A])(f: (Par[A], Par[A]) => Par[A])
  }

  object Par {


    // Promotes a constant value to a parallel computation
    def unit[A](a: => A): Par[A] = ???

    // Combines the results of two parallel computations with a binary function.
    def map2[A](pa: => Par[A], pb: => Par[A])(f: (A, A) => A): Par[A] = ???

    // Marks a computation for concurrent execution
    def fork[A](pa: => Par[A]): Par[A] = ???

    // Wraps its unevaluated argument in a Par and marks for concurrent evaluation.
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    // Extracts a value by actually performing the computation.
    def run[A](pa: Par[A]): A = ???


    // How to represent Par?
    type Par[A] = ExecutorService => Future[A]
  }
}
