package week4

trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
  def show: String = this match {
    case x: Number => x.eval.toString
    case Sum(e1, e2) => e1.show + " + " + e2.show
  }
}
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(name: String) extends Expr

object Expr {
  def show(e: Expr): String = e match {
    case Sum(l:Prod, r) => show(l.e1) + " * (" + show(l.e2) + " + " + show(r) + ")"
    case Sum(l, r) => show(l) + " + " + show(r)
    case Prod(l:Sum, r) => "(" + show(l) + ") * " + show(r)
    case Prod(l, r) => show(l) + " * " + show(r)
    case Var(x) => x.toString
    case x: Number => x.eval.toString
  }
}