package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.BaseDimensions._

object TemperatureConversion {
  val C: DerivedUnit = defineUnit("°C", Temperature)
  val K: DerivedUnit = defineUnit("K", Temperature)
  val F: DerivedUnit = defineUnit("°F", Temperature)

  C.defineConversion(K, (c: Double) => c + 273.15)
  K.defineConversion(C, (k: Double) => k - 273.15)
  F.defineConversion(K, (f: Double) => (f + 459.67) * 5.0/9.0)
  K.defineConversion(F, (k: Double) => k * 9.0/5.0 - 459.67)

  def convert(measurement: Measurement[Double], to: DerivedUnit): Measurement[Double] = {
    measurement.convertTo(K).convertTo(to)
  }

  def main(args: Array[String]): Unit = {
    val measurement = 55.4(K)
    printf("%s = %s = %s\n", convert(measurement, K), convert(measurement, C), convert(measurement, F))
  }
}
