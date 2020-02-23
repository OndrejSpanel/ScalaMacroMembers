import reflect.macros.blackbox
import scala.language.experimental.macros

object Macros {

  def members[T]: Seq[String] = macro members_impl[T]
  def sumMembers[T]: T => Int = macro sumMembers_impl[T]

  def walker[T]: (T, (T => Unit)) => Unit = macro walker_impl[T]

  private def members_list_impl[T: c.WeakTypeTag](c: blackbox.Context) = {
    import c.universe._
    val T = weakTypeOf[T]
    T.decls.filter { f =>
      f.isMethod && f.asMethod.paramLists.isEmpty && f.asMethod.returnType == typeOf[Int]
    }
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
    val T = weakTypeOf[T]
    val members = list.map { m =>
      q"x.$m"
    }
    c.Expr[T => Int](
      q"(x: $T) => Seq(..$members).sum"
    )
  }

  def walker_impl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[(T, (T => Unit)) => Unit] = {
    import c.universe._
    val T = weakTypeOf[T]
    c.Expr[(T, (T => Unit)) => Unit](
      q"(t: $T, f: $T => Unit) => f(t)"
    )
  }
}