package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure._

/**
  * An example demonstrating why radians should NOT be defined as aliases of DimensionlessUnit
  *
  * Dimensionless values can be silently cast to radians, which can lead to undetected unit errors
  */
object DimensionlessRadians {
  val rad: DerivedUnit = DerivedUnit.DimensionlessUnit alias "rad"
  val deg: DerivedUnit = 2.0 * Math.PI / 360.0 * rad alias "°"

  def sin(theta: Measurement[Double]): Double = Math.sin(theta.value(rad))

  def main(args: Array[String]): Unit = {
    printf("If units are omitted, %.3f becomes %.3f because the value is treated as radians\n", sin(90.0(deg)), sin(90.0))
  }
}
