package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

class StagedAdditionSpec extends BaseStagedSpec {
  "StagedAddition" should "perform addition and unit conversion correctly" in {
    val driver = StagedAddition.run

    assertResult(182.5) {
      // 1.5m + 32.5cm = 182.5cm
      driver.f(1.5)
    }
  }

  it should "produce the correct code" in {
    val driver = StagedAddition.run

    assertResult(readExpectedResult("StagedAddition")) {
      driver.code.trim()
    }
  }
}
