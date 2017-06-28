package fpinscala.state

object KaeApp extends App {

  val seed = 1924L

  val initialRng = RNG.Simple(seed)

  println(
    RNG.ints(19)(initialRng)
  )
}
