package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import org.scalatest.Matchers

class StagedDecibelSpec extends BaseStagedSpec with Matchers {
  "A StagedDecibel" should "correctly compute the resulting signal strength" in {
    val driver = StagedDecibelsMain.run

    driver.f(10) should be (0.483 +- 0.005)
  }

  it should "produce the correct code" in {
    val driver = StagedDecibelsMain.run

    assertResult(readExpectedResult("StagedDecibels")) {
      driver.code.trim
    }
  }
}
