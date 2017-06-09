package fpinscala.laziness

object KaeApp extends App {
  println(
    Stream(1, 2, 3, 4).forAll(_ % 1 == 0)
  )

  println(
    Stream(1, 2, 3, 4).takeWhileUsingFoldRight(_ < 4).toList
  )
}
