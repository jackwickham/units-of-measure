package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure.SI._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples.lms_utils._

object StagedUnitBug {
  def driver: DslDriver[(Double, Double), Double] = new DslDriver[(Double, Double), Double] with Dsl with Utils {
    /**
      * Calculate the average force applied when running 100m
      *
      * This code contains a bug, because velocity is used rather than acceleration
      */
    override def snippet(args: Rep[(Double, Double)]): Rep[Double] = {
      val distance = new Measurement[Rep[Double]](unit(100.0), m)
      val time = (args._1)(s)
      val mass = (args._2)(kg)
      val velocity = distance / time
      val force = velocity * mass

      force.value(Derived.N)
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      println(driver.code)
    } catch {
      // No code is emitted when we catch a bug - it's a compile time error
      case DimensionError(_, _) => println("Caught a bug, so no code is emitted")
    }
  }
}
