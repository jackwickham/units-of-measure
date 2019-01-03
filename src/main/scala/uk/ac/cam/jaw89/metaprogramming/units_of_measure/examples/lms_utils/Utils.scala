package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples.lms_utils

import uk.ac.cam.jaw89.metaprogramming.units_of_measure._

trait Utils extends Dsl {

  /**
    * `Provide a Fractional[Rep[Double]]` and `IntegerMultiplyAndExponentiate[Rep[Double]]`
    */
  implicit object RepDblFractional extends Fractional[Rep[Double]] with MultiplyAndExponentiate[Rep[Double]] {
    override def fromInt(x: Int): Rep[Double] = unit(x.toDouble)
    override def compare(x: Rep[Double], y: Rep[Double]): Int = throw new UnsupportedOperationException() // Compare needs to return Int, not Rep[Int]
    override def div(x: Rep[Double], y: Rep[Double]): Rep[Double] = x / y
    override def minus(x: Rep[Double], y: Rep[Double]): Rep[Double] = x - y
    override def negate(x: Rep[Double]): Rep[Double] = -x
    override def plus(x: Rep[Double], y: Rep[Double]): Rep[Double] = x + y
    override def times(x: Rep[Double], y: Rep[Double]): Rep[Double] = x * y
    override def toDouble(x: Rep[Double]): Double = throw new UnsupportedOperationException()
    override def toFloat(x: Rep[Double]): Float = throw new UnsupportedOperationException()
    override def toInt(x: Rep[Double]): Int = throw new UnsupportedOperationException()
    override def toLong(x: Rep[Double]): Long = throw new UnsupportedOperationException()
    override def zero: Rep[Double] = unit(0.0)
    override def one: Rep[Double] = unit(1.0)

    override def times(x: Rep[Double], y: Multiplier): Rep[Double] = x * y
    override def div(x: Rep[Double], y: Multiplier): Rep[Double] = x / y
  }
}
