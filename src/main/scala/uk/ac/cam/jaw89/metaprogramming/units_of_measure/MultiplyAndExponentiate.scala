package uk.ac.cam.jaw89.metaprogramming.units_of_measure

import scala.math.Numeric.DoubleIsFractional
import scala.math.Ordering.DoubleOrdering

trait MultiplyAndExponentiate[A] {
  def times(x: A, y: Multiplier): A
  def div(x: A, y: Multiplier): A

  /**
    * Naive integer power algorithm using multiplication and division
    *
    * @param x Value to raise to a power
    * @param y Power to raise it to
    * @return x^y^
    */
  def pow(x: A, y: Exponent)(implicit frac: Fractional[A]): A = {
    var r: A = frac.one
    if (y < 0) {
      for (_ <- 0 until -y) {
        r = frac.div(r, x)
      }
    } else {
      for (_ <- 0 until y) {
        r = frac.times(r, x)
      }
    }
    r
  }
}

object MultiplyAndExponentiate {

  /**
    * Provide an instance of MeasurementOperators when using Double
    */
  implicit object MultiplierIntegerMultiplyAndExponentiate$Double extends DoubleIsFractional with MultiplyAndExponentiate[Double] with DoubleOrdering
}
