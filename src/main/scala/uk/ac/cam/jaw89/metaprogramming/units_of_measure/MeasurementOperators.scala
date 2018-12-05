package uk.ac.cam.jaw89.metaprogramming.units_of_measure

import scala.math.Numeric.DoubleIsFractional
import scala.math.Ordering.DoubleOrdering

trait MeasurementOperators[A] extends Fractional[A] {
  def times(x: A, y: Multiplier): A
  def div(x: A, y: Multiplier): A

  /**
    * Naive integer power algorithm using multiplication and division
    *
    * @param x Value to raise to a power
    * @param y Power to raise it to
    * @return x^y^
    */
  def pow(x: A, y: Exponent): A = {
    var r: A = one
    if (y < 0) {
      for (_ <- 0 until -y) {
        r = div(r, x)
      }
    } else {
      for (_ <- 0 until y) {
        r = times(r, x)
      }
    }
    r
  }
}

object MeasurementOperators {

  /**
    * Provide an instance of MeasurementOperators when using Double
    */
  implicit object MultiplierMeasurementOperators extends DoubleIsFractional with MeasurementOperators[Double] with DoubleOrdering
}
