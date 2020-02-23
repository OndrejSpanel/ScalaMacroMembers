import reflect.macros.blackbox
import scala.language.experimental.macros

object Macros {

  def hello(): Unit = macro hello_impl

  def hello_impl(c: blackbox.Context)(): c.Expr[Unit] = {
    import c.universe._
    reify { println("Hello World!") }
  }
}