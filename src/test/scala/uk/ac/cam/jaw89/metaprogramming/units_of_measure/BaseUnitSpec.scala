package uk.ac.cam.jaw89.metaprogramming.units_of_measure

class BaseUnitSpec extends TestSpec {
  "A base unit" should "not be equal to anything but itself" in {
    val a = new BaseUnit(Dimension.Dimensionless)
    val b = new BaseUnit(Dimension.Dimensionless)

    assert(a == a)
    assert(a != b)
  }
}
