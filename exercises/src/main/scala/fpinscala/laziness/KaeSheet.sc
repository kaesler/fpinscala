import fpinscala.laziness._

Stream.ones.take(11).toList

Stream.ones.take(12).drop(11).toList

Stream(1, 2, 3, 4).forAll(_ % 2 == 0)

Stream.from(1).takeWhileUsingFoldRight(_ < 24)




