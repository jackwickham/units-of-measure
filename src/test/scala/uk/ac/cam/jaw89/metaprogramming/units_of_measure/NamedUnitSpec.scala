package uk.ac.cam.jaw89.metaprogramming.units_of_measure

import BaseDimensions._

class NamedUnitSpec extends TestSpec {
  "A NamedUnit" should "equal another NamedUnit with the same symbol, base units and multiplier" in {
    val u = new BaseUnit(Length)
    val a = new NamedUnit("z", PowersOf(u -> 1))
    val b = new NamedUnit("z", PowersOf(u -> 1))

    assert(a == b)
  }

  it should "not equal another NamedUnit if the multiplier is different" in {
    val u = new BaseUnit(Length)
    val a = new NamedUnit("z", PowersOf(u -> 1))
    val b = new NamedUnit("z", PowersOf(u -> 1), 10)

    assert(a != b)
  }

  "NamedUnit.dimensions" should "return the combined dimensions of all the base units" in {
    val u1 = new BaseUnit(Length)
    val u2 = new BaseUnit(Time * Mass~^2)
    val u3 = new BaseUnit(Current / Mass)

    val a = new NamedUnit("z", PowersOf(u1 -> -1, u2 -> 1, u3 -> 2))

    assertResult(Length~^-1 * Time~^1 * Current~^2) {
      a.dimensions
    }
  }

  "NamedUnit.toString" should "return its symbol" in {
    val u = new BaseUnit(Length)
    val a = new NamedUnit("abc", PowersOf(u -> 1))

    assertResult("abc") {
      a.toString
    }
  }
}
