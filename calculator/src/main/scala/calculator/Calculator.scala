package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for ((name, s) <- namedExpressions) yield name -> Signal(eval(s(), namedExpressions))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]], path: List[String] = Nil): Double = {
    expr match {
      case Literal(v) => v
      case Plus(a, b) => eval(a, references, path) + eval(b, references, path)
      case Minus(a, b) => eval(a, references, path) - eval(b, references, path)
      case Times(a, b) => eval(a, references, path) * eval(b, references, path)
      case Divide(a, b) => eval(a, references, path) / eval(b, references, path)
      case Ref(name) =>
        if (path.contains(name)) Double.NaN
        else eval(getReferenceExpr(name, references), references, name :: path)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
