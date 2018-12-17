package uk.ac.cam.jaw89.metaprogramming.units_of_measure

import org.scalatest.{BeforeAndAfterEach, Matchers}
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.BaseDimensions._

class DerivedUnitSpec extends TestSpec with Matchers with BeforeAndAfterEach {
  private var ua = defineUnit("a", Length)
  private var ub = defineUnit("b", Length)
  private var uc = defineUnit("c", Length * Time)

  override def beforeEach(): Unit = {
    ua = defineUnit("a", Length)
    ub = defineUnit("b", Length)
    uc = defineUnit("c", Length * Time)
  }

  "defineConversion" should "not reject suitable conversions" in {
    ua.defineConversion(ub, (v: Double) => v * 0.5 + 25.25)
  }

  it should "throw DimensionError if the dimensions differ" in {
    assertThrows[DimensionError] {
      ua.defineConversion(uc, (v: Double) => v * 0.5 + 25.25)
    }
  }

  "convert" should "perform the conversion correctly if it has been set up" in {
    ua.defineConversion(ub, (v: Double) => v * 0.5 + 25.25)

    ua.convert[Double, Double](30.0, ub) should be (40.25 +- 0.0001)
  }

  it should "allow multiple conversions to be registered for different types" in {
    ua.defineConversion(ub, (v: Double) => v * 0.5 + 25.25)
    ua.defineConversion(ub, (v: Int) => v + 5)

    ua.convert[Double, Double](30.0, ub) should be (40.25 +- 0.0001)
    ua.convert[Int, Int](20, ub) should be (25)
  }

  it should "throw NoCompatibleConversionsException if the type is different to the defined conversions" in {
    ua.defineConversion(ub, (v: Double) => v * 0.5 + 25.25)

    assertThrows[NoCompatibleConversionsException] {
      ua.convert[Int, Int](30, ub)
    }
  }

  it should "throw NoUnitConversionsDefinedException if no unit conversions have been defined for the units" in {
    assertThrows[NoUnitConversionsDefinedException] {
      ua.convert[Int, Int](30, ub)
    }
  }

  it should "throw DimensionError if the dimensions of the units differ" in {
    assertThrows[DimensionError] {
      ua.convert[Double, Double](30.0, uc) should be(40.25 +- 0.0001)
    }
  }
}
