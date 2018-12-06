package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure._

class UnitBugSpec extends TestSpec {
  "UnitBug" should "throw a DimensionError" in {
    assertThrows[DimensionError] {
      UnitBug.calculateForce(12.1, 70.0)
    }
  }
}
