package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import org.scalatest.Matchers
import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples.DimensionlessRadians.{Unsafe, Safe}

class DimensionlessRadiansSpec extends TestSpec with Matchers {
  private val sinPi_4 = 0.7071067811865476

  "DimensionlessRadians.Unsafe.sin" should "work with radians correctly" in {
    Unsafe.sin((Math.PI/4)(Unsafe.rad)) should be (sinPi_4 +- 0.05)
  }

  it should "work with degrees correctly" in {
    Unsafe.sin(45.0(Unsafe.deg)) should be (sinPi_4 +- 0.05)
  }

  it should "incorrectly allow a value with unspecified units to be passed" in {
    Unsafe.sin(Math.PI/4) should be (sinPi_4 +- 0.05)
  }

  "DimensionlessRadians.Safe.sin" should "work with radians correctly" in {
    Safe.sin((Math.PI/4)(Safe.rad)) should be (sinPi_4 +- 0.05)
  }

  it should "work with degrees correctly" in {
    Safe.sin(45.0(Safe.deg)) should be (sinPi_4 +- 0.05)
  }

  it should "throw a NoImplicitConversionsAvailableException when a value with unspecified units is passed" in {
    assertThrows[NoImplicitConversionsAvailableException] {
      Safe.sin(Math.PI / 4)
    }
  }
}
