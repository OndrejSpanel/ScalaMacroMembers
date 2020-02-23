import Macros._

object Main extends App {
  class X(a: Int = 1, b: Int = 2, c: Int = 3) {
    val xa = a
    val xb = b
    val xc = c
  }
  val m = members[X]
  val x = new X()
  val y = new X(10, 100, 1000)
  println(s"X members: $m")
  val sum = sumMembers[X]
  println(s"x sum: ${sum(x)}")
  println(s"y sum: ${sum(y)}")

  trait Node

  case class Root(children: Seq[Node]) extends Node
  case class Leaf() extends Node

  val tree = Root(Seq(Leaf(), Leaf(), Root(Seq(Leaf()))))
  val walk = walker[Root]
  walk(tree, x => println(x.getClass.getSimpleName))
}
