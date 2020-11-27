package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] =
    Signal(Math.pow(b(), 2.0) - 4 * a() * c())

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      val nowdelta:Double = delta()
      if (nowdelta > 0.0){
          val sqrt_delta = Math.sqrt(nowdelta)
          Set(
            (-b() + sqrt_delta) / (2 * a()),
            (-b() - sqrt_delta) / (2 * a())
          )
      } else Set(0.0)
    }
}
