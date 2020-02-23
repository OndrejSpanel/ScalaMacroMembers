import reflect.macros.blackbox
import scala.language.experimental.macros

object Macros {

  def members[T]: Seq[String] = macro members_impl[T]
  def sumMembers[T]: T => Int = macro sumMembers_impl[T]

  def walker[B, T <: B]: (T, B => Unit) => Unit = macro walker_impl[B, T]
  def transformer[B, T <: B]: (T, B => B) => Unit = macro transformer_impl[B, T]

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


  def walker_impl[B: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context): c.Expr[(T, B => Unit) => Unit] = {
    import c.universe._

    val iterable = typeOf[Iterable[Any]]
    val iterableClass = iterable.typeSymbol

    val T = weakTypeOf[T]
    val B = weakTypeOf[B]
    def isSeqB(returnType: Type) = {
      returnType <:< iterable && returnType.baseType(iterableClass).typeArgs.headOption.exists(_ <:< B)
    }
    val dive = T.decls.collect {
      case f if f.isMethod && f.asMethod.paramLists.isEmpty && f.asMethod.isGetter && f.asMethod.returnType <:< B =>
        q"f(t.$f)"
      case f if f.isMethod && f.asMethod.paramLists.isEmpty && f.asMethod.isGetter && isSeqB(f.asMethod.returnType) =>
        q"t.$f.foreach(f)"
    }

    val r = q"(t: $T, f: $B => Unit) => {..$dive}"
    c.Expr(r)
  }

  def transformer_impl[B: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context): c.Expr[(T, B => B) => Unit] = {
    import c.universe._
    val T = weakTypeOf[T]
    val B = weakTypeOf[B]
    val iterable = typeOf[Iterable[Any]]
    val iterableClass = iterable.typeSymbol
    def isSeqB(returnType: Type) = {
      returnType <:< iterable && returnType.baseType(iterableClass).typeArgs.headOption.exists(_ <:< B)
    }
    val dive = T.decls.collect {
      case f if f.isMethod && f.asMethod.paramLists.isEmpty && f.asMethod.isGetter && f.asMethod.setter != NoSymbol && f.asMethod.returnType <:< B =>
        val s = f.asMethod.setter
        val MT = f.asMethod.returnType
        q"t.$s(f(t.$f).asInstanceOf[$MT])"
      case f if f.isMethod && f.asMethod.paramLists.isEmpty && f.asMethod.isGetter && f.asMethod.setter != NoSymbol && isSeqB(f.asMethod.returnType) =>
        val s = f.asMethod.setter
        val MT = f.asMethod.returnType.baseType(iterableClass).typeArgs.head
        q"t.$s(t.$f.map(m => f(m).asInstanceOf[$MT]))"
    }

    val r = q"(t: $T, f: $B => $B) => {..$dive}"
    c.Expr(r)
  }

}