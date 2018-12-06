package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure.SI._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples.lms_utils._

/**
  * https://en.wikipedia.org/wiki/Mars_Climate_Orbiter
  *
  * The Mars Climate Orbiter crashed in 1999 because one part of the system used pound-force rather than SI newtons to
  * calculate the impulse that had been created by firing the thrusters
  */

object StagedMarsClimateOrbiter {

  trait NASA extends Dsl with Utils {

    /**
      * A misbehaving orbiter, which returns the impulse in lbf*s rather than N*s
      */
    class Orbiter {
      private val ft = 0.3048 * m alias "ft"
      private val lbf = 4.448222 * Derived.N alias "lbf"

      val mass: Measurement[Rep[Double]] = new Measurement(unit(122.0), kg)

      /**
        * The software spec said that we must use SI, but the thruster's force has been defined in terms of pound-force
        */
      private val thrusterForce: Measurement[Rep[Double]] = new Measurement(unit(189.0), lbf)

      private def fireThrusters(duration: Measurement[Rep[Double]]): Unit = {
        // Imagination still needed, sorry
      }

      /**
        * Fire the thrusters and return the impulse created
        */
      def fireThrustersAndCalculateImpulse(duration: Measurement[Rep[Double]]): Measurement[Rep[Double]] = {
        fireThrusters(duration)
        duration * thrusterForce
      }
    }

  }

  def run: DslDriver[Double, Double] = new DslDriver[Double, Double] with NASA {
    override def snippet(durationSeconds: Rep[Double]): Rep[Double] = {
      val orbiter = new Orbiter
      val duration = durationSeconds(s)
      // Fire the thrusters to correct for some rotation. We want the result in Ns, but will be given it in lbf*s
      val impulse = orbiter.fireThrustersAndCalculateImpulse(duration)
      // Calculate the orbiter's change in velocity based on the impulse
      val Δv = impulse / orbiter.mass
      // Now record the change in velocity
      // We still get the velocity in ms^-1, regardless of the assumptions that we made about the value returned from
      // the other code
      Δv.value(m/s)
    }
  }

  def main(args: Array[String]): Unit = {
    val r = run
    println(r.code)
  }
}
