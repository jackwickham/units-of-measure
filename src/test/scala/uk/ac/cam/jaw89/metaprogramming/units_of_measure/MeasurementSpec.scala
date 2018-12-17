package uk.ac.cam.jaw89.metaprogramming.units_of_measure

import SI._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.BaseDimensions.Length

class MeasurementSpec extends TestSpec {
  "Measurement.toString" should "strigify the value and unit" in {
    val v = 100.0(m/s)
    assertResult("100.0 m s^-1") { v.toString }
  }

  "Measurement.===" should "require the values and arguments to be the same" in {
    val v = 100.0(m/s)
    val km = 1000.0 * m alias "km"
    assert(v !== 0.1(km/s))
    assert(v !== 100.0(km/s))
    assert(v !== 0.1(m/s))
    assert(v === 100.0(m/s))
  }

  "Measurement.=~" should "convert units and test for equality" in {
    val v = 100.0(m/s)
    val km = 1000.0 * m alias "km"
    assert(v =~ 0.1(km/s))
    assert(v !=~ 100.0(km/s))
  }

  it should "throw an exception if the units are inconvertible" in {
    val v = 100.0(m/s)
    assertThrows[DimensionError] {
      v =~ 0.1(m~^2)
    }
    assertThrows[DimensionError] {
      v !=~ 100.0(m~^2)
    }
  }

  "Measurement.equals" should "convert units when testing for equality" in {
    val v = 100.0(m/s)
    val km = 1000.0 * m
    assert(v == 0.1(km/s))
    assert(v != 100.0(km/s))
  }

  it should "treat inconvertible units as not matching, without throwing an exception" in {
    val v = 100.0(m/s)
    assertResult(false) {
      v == 0.1(m~^2)
    }
    assertResult(true) {
      v != 100.0(m~^2)
    }
  }

  "Measurement.in" should "not do anything if the units are the same" in {
    val v = 100.0(m/s)
    assertResult(v) {
      v in m/s
    }
  }

  it should "just change the units when an alias is used" in {
    val mps = m/s alias "mps"
    val v = 100.0(m/s)
    assertResult(100.0(mps)) {
      v in mps
    }
  }

  it should "multiply and divide the units correctly when a multiplier is involved" in {
    val cm = 0.01 * m alias "cm"
    val min = 60.0 * s alias "min"
    val v = 10.0(m/s~^2)
    assertResult(3600000.0(cm/min~^2)) {
      v in cm/min~^2
    }
  }

  it should "throw a NoUnitConversionsDefinedException if the dimensions are the same but the base units differ and there are no available conversions" in {
    val v = 273.15(K)
    assertThrows[NoUnitConversionsDefinedException] {
      v in Derived.celcius
    }
  }

  it should "throw a DimensionError if the dimensions differ" in {
    val v = 100.0(m)
    assertThrows[DimensionError] {
      v in s
    }
  }

  it should "use DerivedUnit.convert if no implicit conversion is available" in {
    val a = defineUnit("a", Length)
    val b = defineUnit("b", Length)

    val v = 9.0(a)

    a.defineConversion(b, (v: Double) => v + 1.0)

    assertResult(10.0(b)) {
      v in b
    }
  }

  it should "handle result type errors with explicit conversions, and throw an exception" in {
    val a = defineUnit("a", Length)
    val b = defineUnit("b", Length)

    val v = 9.0(a)

    a.defineConversion(b, (_: Double) => 10)

    assertThrows[IncorrectConversionResultTypeException] {
      v in b
    }
  }
}
