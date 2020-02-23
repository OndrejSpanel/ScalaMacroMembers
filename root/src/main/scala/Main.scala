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

  case class Root(children: Seq[Node]) extends Node {
    override def toString = s"Root(${children.size})"
  }
  case class Bi(left: Node, var right: Node) extends Node
  case class Mixed(left: Node, var right: Seq[Node]) extends Node
  case class Leaf(id: String) extends Node

  val tree = Root(Seq(Leaf("1_a"), Leaf("1_b"), Root(Seq(Leaf("2_c"), Bi(Leaf("bi_d"), Leaf("bi_e"))))))
  val bi = Bi(Leaf("bi_x"), Leaf("bi_y"))
  val mixed = Mixed(Leaf("mix_a"), Seq(Leaf("mix_b1"), Leaf("mix_b2")))
  val walk = walker[Node, Root]
  val walkBi = walker[Node, Bi]
  val walkMixed = walker[Node, Mixed]
  walk(tree, x => println(x))
  walkBi(bi, x => println(x))
  walkMixed(mixed, x => println(x))
}
