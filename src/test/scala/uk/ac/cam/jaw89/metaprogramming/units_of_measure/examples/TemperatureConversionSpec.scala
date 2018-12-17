package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import org.scalatest.Matchers
import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples.TemperatureConversion.{C, F, K}

class TemperatureConversionSpec extends TestSpec with Matchers {
  "convert" should "perform conversion between C and F" in {
    TemperatureConversion.convert(37.0(C), F).value(F) should be (98.6 +- 0.0001)
  }

  it should "perform conversion between K and C" in {
    TemperatureConversion.convert(56.8(K), C).value(C) should be (-216.35 +- 0.0001)
  }
}
