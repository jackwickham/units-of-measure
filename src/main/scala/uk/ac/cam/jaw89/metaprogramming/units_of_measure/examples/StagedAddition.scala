package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.SI._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples.lms_utils._

import scala.lms.common._

trait Add extends Dsl {
  implicit object RepDblFractional extends Fractional[Rep[Double]] with IntegerMultiplyAndExponentiate[Rep[Double]] {
    override def fromInt(x: Int): Rep[Double] = ???
    override def compare(x: Rep[Double], y: Rep[Double]): Int = ???
    override def div(x: Rep[Double], y: Rep[Double]): Rep[Double] = x / y
    override def minus(x: Rep[Double], y: Rep[Double]): Rep[Double] = x - y
    override def negate(x: Rep[Double]): Rep[Double] = -x
    override def plus(x: Rep[Double], y: Rep[Double]): Rep[Double] = x + y
    override def times(x: Rep[Double], y: Rep[Double]): Rep[Double] = x * y
    override def toDouble(x: Rep[Double]): Double = ???
    override def toFloat(x: Rep[Double]): Float = ???
    override def toInt(x: Rep[Double]): Int = ???
    override def toLong(x: Rep[Double]): Long = ???
    override def zero: Rep[Double] = unit(0.0)
    override def one: Rep[Double] = unit(1.0)

    override def times(x: Rep[Double], y: Multiplier): Rep[Double] = x * y
    override def div(x: Rep[Double], y: Multiplier): Rep[Double] = x / y
  }

  def add: Rep[Double => Double] = fun {
    (n: Rep[Double]) => {
      val x = new Measurement[Rep[Double]](n, m)
      val y = new Measurement[Rep[Double]](unit(3.0), m)
      (x + y).value(m * 0.01)
    }
  }
}

class StagedAddition {
  def run: DslDriver[Double, Double] = new DslDriver[Double, Double] with Add {
    override def snippet(n: Rep[Double]): Rep[Double] = add(n)
  }
}

object Addition {
  def main(args: Array[String]): Unit = {
    val s = new StagedAddition
    val r = s.run
    println(r.code)
  }
}

