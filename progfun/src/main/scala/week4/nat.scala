package week4

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def sucessor = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
  def get_number(n: Nat, acc:Int = 0): Int =
    if (n.isZero) acc
    else get_number(n.predecessor, acc + 1)
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predecessor")
  def +(that: Nat) = that
  def -(that: Nat) = if (that.isZero) this else throw new Error("negative number")
}

class Succ(n: Nat) extends Nat {
  val number:Int = get_number(this)
  def isZero = false
  def predecessor = n
  def +(that: Nat) = new Succ(n + that)
  def -(that: Nat) = n - that.predecessor

  override def toString = {
    String.valueOf(number) + " => " +
      ((1 to number) fold ("Zero")) ( (s,_) => "Succ(" + s + ")")
  }
}