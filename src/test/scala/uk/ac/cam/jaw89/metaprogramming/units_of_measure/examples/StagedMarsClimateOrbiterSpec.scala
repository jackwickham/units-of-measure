package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import org.scalatest.Matchers

class StagedMarsClimateOrbiterSpec extends BaseStagedSpec with Matchers {
  "A StagedMarsClimateOrbiter" should "correctly compute the velocity in ms^-1" in {
    val driver = StagedMarsClimateOrbiter.run

    driver.f(10) should be (68.91 +- 0.1)
  }

  it should "produce the correct code" in {
    val driver = StagedMarsClimateOrbiter.run

    assertResult(readExpectedResult("StagedMarsClimateOrbiter")) {
      driver.code.trim
    }
  }
}
