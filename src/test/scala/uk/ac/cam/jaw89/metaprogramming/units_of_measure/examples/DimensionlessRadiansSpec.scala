package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import org.scalatest.Matchers
import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples.DimensionlessRadians.{deg, rad, sin}

class DimensionlessRadiansSpec extends TestSpec with Matchers {
  private val sinPi_4 = 0.7071067811865476

  "DimensionlessRadians.sin" should "work with radians correctly" in {
    sin((Math.PI/4)(rad)) should be (sinPi_4 +- 0.05)
  }

  it should "work with degrees correctly" in {
    sin(45.0(deg)) should be (sinPi_4 +- 0.05)
  }

  it should "incorrectly allow a value with unspecified units to be passed" in {
    sin(Math.PI/4) should be (sinPi_4 +- 0.05)
  }
}
