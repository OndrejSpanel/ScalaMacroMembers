import Macros._

object Main extends App {
  class X {
    val xa = 1
    val xb = 2
    val xc = 3
  }
  val m = members[X]
  println(s"X members: $m")
}
