package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.SI._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples.lms_utils._

object StagedAddition {
  def run: DslDriver[Double, Double] = new DslDriver[Double, Double] with Dsl with Utils {
    override def snippet(n: Rep[Double]): Rep[Double] = {
      val cm = m * 0.01
      val x = n(m)
      val y = new Measurement[Rep[Double]](unit(32.5), cm)
      (x + y).value(cm)
    }
  }

  def main(args: Array[String]): Unit = {
    val r = run
    println(r.code)
  }
}
