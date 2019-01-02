package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.BaseDimensions._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples.lms_utils._

import scala.language.implicitConversions

trait StagedDecibels extends Dsl with Utils {
  case class BoxedRep[T](r: Rep[T])
  implicit def autoBoxRep[T](r: Rep[T]): BoxedRep[T] = BoxedRep(r)
  implicit def autoUnboxRep[T](r: BoxedRep[T]): Rep[T] = r.r

  case class Decibel(v: Rep[Double])

  implicit object DecibelFractional extends Fractional[Decibel] with IntegerMultiplyAndExponentiate[Decibel] with BinaryFractional[Decibel, Decibel] with BinaryNumeric[Decibel, Decibel] {
    override def fromInt(powerRatio: Int): Decibel = Decibel(unit(10.0) * Math.log10(unit(powerRatio.toDouble)))
    def fromRepDouble(powerRatio: Rep[Double]): Decibel = Decibel(unit(10.0) * Math.log10(powerRatio))
    override def compare(x: Decibel, y: Decibel): Int = throw new UnsupportedOperationException()
    override def div(x: Decibel, y: Decibel): Decibel = Decibel(x.v - y.v)
    override def minus(x: Decibel, y: Decibel): Decibel = Decibel(unit(10.0) * Math.log10(Math.pow(unit(10.0), x.v / unit(10.0)) - Math.pow(unit(10.0), y.v / unit(10.0))))
    override def plus(x: Decibel, y: Decibel): Decibel = Decibel(unit(10.0) * Math.log10(Math.pow(unit(10.0), x.v / unit(10.0)) + Math.pow(unit(10.0), y.v / unit(10.0))))
    override def times(x: Decibel, y: Decibel): Decibel = Decibel(x.v + y.v)
    override def negate(x: Decibel): Decibel = Decibel(unit(-1.0) * x.v)
    def toRepDouble(x: Decibel): Rep[Double] = Math.pow(unit(10.0), x.v / unit(10.0))
    override def toDouble(x: Decibel): Double = throw new UnsupportedOperationException()
    override def toFloat(x: Decibel): Float = throw new UnsupportedOperationException()
    override def toInt(x: Decibel): Int = throw new UnsupportedOperationException()
    override def toLong(x: Decibel): Long = throw new UnsupportedOperationException()

    override def times(x: Decibel, y: Multiplier): Decibel = times(x, fromRepDouble(unit(y)))
    override def div(x: Decibel, y: Multiplier): Decibel = div(x, fromRepDouble(unit(y)))
  }

  def calculateEffectiveSignal(powerDecibels: Rep[Double]): Rep[Double] = {
    val dBm = defineUnit("dBm", Mass * Length~^2 * Time~^-3)

    val powerMeasureDBm = Decibel(powerDecibels)(dBm)

    val transmitterNoise = Decibel(5.0)(dBm)
    val attenuationPerMetre = 0.5
    val distance = 6
    val attenuationRatio = Math.pow(attenuationPerMetre, distance)
    val receiverNoise = Decibel(-0.4)(dBm)

    val resultingSignal = (powerMeasureDBm + transmitterNoise) * DecibelFractional.fromRepDouble(attenuationRatio) + receiverNoise

    resultingSignal.value(dBm).v
  }
}

object StagedDecibelsMain {
  def run: DslDriver[Double, Double] = new DslDriver[Double, Double] with StagedDecibels {
    override def snippet(powerMW: Rep[Double]): Rep[Double] = {
      calculateEffectiveSignal(powerMW)
    }
  }

  def main(args: Array[String]): Unit = {
    val r = run
    println(r.code)
  }
}
