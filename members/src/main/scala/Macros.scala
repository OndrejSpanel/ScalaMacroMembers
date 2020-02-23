import reflect.macros.blackbox
import scala.language.experimental.macros

object Macros {

  def members[T]: Seq[String] = macro members_impl[T]

  def members_impl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[Seq[String]] = {
    import c.universe._
    val tType = weakTypeOf[T]
    val list = tType.decls.filter { f =>
      println("f " + f)
      f.isTerm && f.name.toString.startsWith("x") && !f.name.toString.endsWith(" ")
    }.map { m =>
      println("m " + m)
      m.name.decodedName.toString
    }.toSeq
    println("list "+list)
    val expr = list.map("\"" + _ + "\"").mkString("Seq(", "," , ")")
    println(expr)
    val enums = List(fq"x <- list")
    c.Expr[Seq[String]](
      //Literal("a")
      //q"$expr"
      //q"for (..$enums) yield i"
      q"""Seq("a")"""
    )
  }


  def sumMembers[T]: T => Int = macro sumMembers_impl[T]

  def sumMembers_impl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[T => Int] = {
    ???
  }
}