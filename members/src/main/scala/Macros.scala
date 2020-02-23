import reflect.macros.blackbox
import scala.language.experimental.macros

object Macros {

  def members[T]: Seq[String] = macro members_impl[T]
  def sumMembers[T]: T => Int = macro sumMembers_impl[T]

  private def members_list_impl[T: c.WeakTypeTag](c: blackbox.Context) = {
    import c.universe._
    val tType = weakTypeOf[T]
    val list = tType.decls.filter { f =>
      f.isMethod && f.asMethod.paramLists.isEmpty && f.asMethod.returnType == typeOf[Int]
    }
    list
  }
  def members_impl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[Seq[String]] = {
    import c.universe._
    val list = members_list_impl[T](c)
    val memberNames = list.map(_.name.decodedName.toString)
    c.Expr[Seq[String]](
      q"Seq(..$memberNames)"
    )
  }

  def sumMembers_impl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[T => Int] = {
    import c.universe._
    val list = members_list_impl[T](c)
    val tType = weakTypeOf[T]
    val members = list.map { m =>
      q"x.$m"
    }
    c.Expr[T => Int](
      q"(x: $tType) => Seq(..$members).sum"
    )
  }
}