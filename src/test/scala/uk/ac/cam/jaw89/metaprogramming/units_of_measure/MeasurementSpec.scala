package uk.ac.cam.jaw89.metaprogramming.units_of_measure

import SI._

class MeasurementSpec extends TestSpec {
  "Measurement.toString" should "strigify the value and unit" in {
    val v = 100(m/s)
    assertResult("100 m s^-1") { v.toString }
  }

  "Measurement.equals" should "require the values and arguments to be the same" in {
    val v = 100.0(m/s)
    val km = 1000.0 * m alias "km"
    assert(v != 0.1(km/s))
    assert(v != 100.0(km/s))
    assert(v != 0.1(m/s))
    assert(v == 100.0(m/s))
  }

  "Measurement.=~" should "convert units and test for equality" in {
    val v = 100.0(m/s)
    val km = 1000.0 * m alias "km"
    assert(v =~ 0.1(km/s))
    assert(v !=~ 100.0(km/s))
  }

  "Measurement.as" should "not do anything if the units are the same" in {
    val v = 100.0(m/s)
    assertResult(v) {
      v as m/s
    }
  }

  it should "just change the units when an alias is used" in {
    val mps = m/s alias "mps"
    val v = 100.0(m/s)
    assertResult(100.0(mps)) {
      v as mps
    }
  }

  it should "multiply and divide the units correctly when a multiplier is involved" in {
    val cm = 0.01 * m alias "cm"
    val min = 60.0 * s alias "min"
    val v = 10.0(m/s~^2)
    assertResult(3600000.0(cm/min~^2)) {
      v as cm/min~^2
    }
  }

  it should "throw a UnitConversionException if the dimensions are the same but the units can't be conveted between" in {
    val v = 273.15(K)
    assertThrows[UnitConversionException] {
      v as Derived.celcius
    }
  }

  it should "throw a DimensionError if the dimensions differ" in {
    val v = 100.0(m)
    assertThrows[DimensionError] {
      v as s
    }
  }
}
