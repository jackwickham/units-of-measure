package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure.SI._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples.Vectors._

class VectorsSpec extends TestSpec {
  "differentiate" should "perform numerical differentiation correctly" in {
    val values = List(
      Vec3(5.0, 6.0, 7.0)(m),
      Vec3(10.0, 11.5, 6.0)(m),
      Vec3(1.2, 1.2, 0.7)(10.0 * m)
    )
    val interval = 0.5(s)

    val expected = List(
      Vec3(10.0, 11.0, -2.0)(m/s),
      Vec3(4.0, 1.0, 2.0)(m/s)
    )

    assertResult(expected) {
      differentiate(values, interval)
    }
  }

  it should "fail on lists with different dimensioned elements" in {
    val values = List(
      Vec3(5.0, 6.0, 7.0)(m),
      Vec3(10.0, 11.5, 6.0)(m),
      Vec3(1.2, 1.2, 0.7)(kg)
    )
    val interval = 0.5(s)

    assertThrows[DimensionError] {
      differentiate(values, interval)
    }
  }
}
