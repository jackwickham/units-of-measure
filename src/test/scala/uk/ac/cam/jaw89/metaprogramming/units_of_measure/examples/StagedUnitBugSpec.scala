package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import org.scalatest.Matchers
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.DimensionError

class StagedUnitBugSpec extends BaseStagedSpec with Matchers {
  "A StagedMarsClimateOrbiter" should "throw a DimensionError at compile time" in {
    assertThrows[DimensionError] {
      val driver = StagedUnitBug.driver
      driver.code
    }
  }

  it should "throw a DimensionError if called directly" in {
    assertThrows[DimensionError] {
      val driver = StagedUnitBug.driver
      driver.f(25.4, 123.4)
    }
  }
}
