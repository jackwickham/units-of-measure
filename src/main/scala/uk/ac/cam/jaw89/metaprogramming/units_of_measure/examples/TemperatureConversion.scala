package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.BaseDimensions._

object TemperatureConversion {
  private val C = defineUnit("C", Temperature)
  private val K = defineUnit("K", Temperature)
  private val F = defineUnit("F", Temperature)

  /**
    * Define a safe abstraction on top of units to convert between measurements with different base units
    */
  class Conversion(from: DerivedUnit, to: DerivedUnit, multiply: Double, add: Double) {
    if (from.dimensions != to.dimensions) {
      throw DimensionError(from.dimensions, to.dimensions)
    }
    if (from.canConvertTo(to)) {
      // They can be implicitly converted, so don't allow explicit conversion
      throw InvalidConversionException()
    }

    def convert(measurement: Measurement[Double]): Measurement[Double] = {
      if (measurement.canConvertTo(from)) {
        new Measurement[Double](measurement.value(from) * multiply + add, to)
      } else if (measurement.canConvertTo(to)) {
        new Measurement[Double]((measurement.value(to) - add) / multiply, from)
      } else {
        throw InvalidConversionException()
      }
    }
  }
  case class InvalidConversionException() extends Exception()

  private val conversionCK = new Conversion(C, K, 1.0, 273.15)
  private val conversionFK = new Conversion(F, K, 5.0/9.0, 273.15 - (32.0 * 5.0 / 9.0))

  def convert(measurement: Measurement[Double], to: DerivedUnit): Measurement[Double] = {
    val inK = if (measurement.canConvertTo(C)) {
      conversionCK.convert(measurement)
    } else if (measurement.canConvertTo(F)) {
      conversionFK.convert(measurement)
    } else {
      // If this fails we weren't given a temperature in the first place
      measurement.in(K)
    }

    if (to.canConvertTo(C)) {
      conversionCK.convert(measurement)
    } else if (to.canConvertTo(F)) {
      conversionFK.convert(measurement)
    } else {
      // If this fails, the target wasn't a temperature that we can convert
      measurement.in(K)
    }
  }

  def main(args: Array[String]): Unit = {
    val measurement = 55.4(K)
    printf("%s = %s = %s\n", convert(measurement, K), convert(measurement, C), convert(measurement, F))
  }
}
