package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure._

/**
  * An example demonstrating why radians should NOT be defined as aliases of DimensionlessUnit
  *
  * Dimensionless values can be silently cast to radians, which can lead to undetected unit errors
  */
object DimensionlessRadians {
  object Unsafe {
    val rad: DerivedUnit = DerivedUnit.DimensionlessUnit alias "rad"
    val deg: DerivedUnit = 2.0 * Math.PI / 360.0 * rad alias "°"

    def sin(theta: Measurement[Double]): Double = Math.sin(theta.value(rad))
  }
  object Safe {
    val rad: DerivedUnit = defineUnit("rad", BaseDimensions.Dimensionless)
    val deg: DerivedUnit = 2.0 * Math.PI / 360.0 * rad alias "°"

    def sin(theta: Measurement[Double]): Double = Math.sin(theta.value(rad))
  }


  def main(args: Array[String]): Unit = {
    printf("If units are omitted, %.3f becomes %.3f in the unsafe system, because the value is treated as radians\n",
           Unsafe.sin(90.0(Unsafe.deg)), Unsafe.sin(90.0))
    // The safe version still allows degrees to be used
    Safe.sin(90.0(Safe.deg))
    // But will throw an exception when unannotated values are used
    try {
      Safe.sin(90.0)
      println("Safe version failed")
    } catch {
      case e: NoImplicitConversionsAvailableException => println("The safe version successfully prevented an error")
    }
  }
}
