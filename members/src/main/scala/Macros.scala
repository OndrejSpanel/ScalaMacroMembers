import reflect.macros.blackbox
import scala.language.experimental.macros

object Macros {

  def members[T]: Seq[String] = macro members_impl[T]

  def members_impl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[Seq[String]] = {
    import c.universe._
    val tType = weakTypeOf[T]
    val list = tType.decls.filter { f =>
      f.isMethod && f.asMethod.paramLists.isEmpty && f.asMethod.returnType == typeOf[Int]
    }.map { m =>
      m.name.decodedName.toString
    }
    c.Expr[Seq[String]](
      q"""Seq(..$list)"""
    )
  }


  def sumMembers[T]: T => Int = macro sumMembers_impl[T]

  def sumMembers_impl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[T => Int] = {
    ???
  }
}