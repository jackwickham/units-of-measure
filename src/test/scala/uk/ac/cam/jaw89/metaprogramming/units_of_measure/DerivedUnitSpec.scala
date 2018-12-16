package uk.ac.cam.jaw89.metaprogramming.units_of_measure

import uk.ac.cam.jaw89.metaprogramming.units_of_measure.SI.K
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.SI.Derived.celcius

import org.scalatest.Matchers

class DerivedUnitSpec extends TestSpec with Matchers {
  "convert" should "work" in {
    celcius.defineConversion(K, (v: Double) => v + 273.15)

    celcius.convert[Double, Double](26.85, K) should be (300.0 +- 0.0001)
  }
}
