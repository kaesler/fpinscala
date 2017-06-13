package fpinscala.laziness

object KaeApp extends App {
  println(
    Stream(1, 2, 3, 4).forAll(_ % 1 == 0)
  )

  println(
    Stream(1, 2, 3, 4).takeWhileUsingFoldRight(_ < 4).toList
  )
  println(
    Stream(1, 2, 3, 4).map(_ * 2).toList
  )
  println(
    Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList
  )
  println(
    Stream(1, 2, 3, 4).append(Stream(5, 6, 7, 8)).toList
  )
  println(
    Stream(1, 2, 3, 4).startsWith(Stream.empty)
  )
  println(
    Stream.empty.startsWith(Stream.empty)
  )
  println(
    Stream(1, 2, 3, 4).startsWith(Stream(1, 2))
  )
  println(
    Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3, 4))
  )
  println(
    Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3, 4, 5))
  )
  println(
    Stream.constant("a").take(20).toList
  )
  println(
    Stream.fibs.take(10).toList
  )
  println(
    Stream.onesUsingUnfold.take(8).toList
  )
  println(
    Stream.fibsUsingUnfold.take(10).toList
  )
}
