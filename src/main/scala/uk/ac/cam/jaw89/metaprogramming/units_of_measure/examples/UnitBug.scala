package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.SI._

object UnitBug {
  /**
    * Calculate the average force applied when running 100m
    *
    * This code contains a bug, because velocity is used rather than acceleration
    */
  def calculateForce(timeS: Double, massKg: Double): Double = {
    val distance = 100.0(m)
    val time = timeS(s)
    val mass = massKg(kg)
    val velocity = distance / time
    val force = velocity * mass

    force.value(Derived.N)
  }

  def main(args: Array[String]): Unit = {
    try {
      printf("Usain Bolt exerted an average force of %.2f N", calculateForce(9.58, 94.0))
    } catch {
      case DimensionError(_, _) => println("Caught a bug")
    }
  }
}
