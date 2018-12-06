package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import org.scalatest.Matchers
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.TestSpec

class MarsClimateOrbiterSpec extends TestSpec with Matchers {
  "A Mars Climate Orbiter" should "compute the change in velocity using m s^-1" in {
    // 189 lbf = 840.714 N
    // 840.714 N * 10 s / 122.0 kg = 68.91 ms^-1
    MarsClimateOrbiter.run(10) should be (68.91 +- 0.1)
  }
}
