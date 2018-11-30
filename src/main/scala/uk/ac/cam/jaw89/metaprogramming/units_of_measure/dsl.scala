package uk.ac.cam.jaw89.metaprogramming.units_of_measure

object UnitsOfMeasure {
  import scala.language.implicitConversions


  def mapCombine[A, B](x: Map[A, B], y: Map[A, B], yMultiplier: B)(implicit num: Numeric[B]): Map[A, B] = {
    // https://gist.github.com/davidandrzej/5413350 with added generality
    val x0 = x.withDefaultValue(num.zero)
    val y0 = y.withDefaultValue(num.zero)
    val keys = x.keySet.union(y.keySet)
    keys.map{ k => k -> num.plus(x0(k), num.times(y0(k), yMultiplier))}.toMap
  }

  def mapCombine[A, B](x: Map[A, B], y: Map[A, B])(implicit num: Numeric[B]): Map[A, B] = mapCombine(x, y, num.one)

  /**
    * A dimension (a unit's "type")
    *
    * Acceleration is represented by Dimension(Distance -> 1, Time -> -2)
    */
  class Dimension private (dimensions: Map[BaseDimension, Int]) {
    val baseDimensions: Map[BaseDimension, Int] = normalise(dimensions)

    def *(other: Dimension): Dimension = new Dimension(mapCombine(baseDimensions, other.baseDimensions))
    def /(other: Dimension): Dimension = new Dimension(mapCombine(baseDimensions, other.baseDimensions, -1))
    def ^(power: Int): Dimension = new Dimension(baseDimensions.mapValues(v => v * power))

    private def normalise(dimensions: Map[BaseDimension, Int]): Map[BaseDimension, Int] = dimensions.filter(e => e._2 != 0)

    def this(baseDimension: BaseDimension) = this(Map(baseDimension -> 1))

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: Dimension => baseDimensions.equals(other.baseDimensions)
        case _ => false
      }
    }

    override def hashCode(): Int = baseDimensions.hashCode()
  }

  /**
    * Base dimensions are the base SI units
    */
  sealed abstract class BaseDimension

  case object Distance extends BaseDimension
  case object Mass extends BaseDimension
  case object Time extends BaseDimension

  case class UnitOfMeasure(dimension: Dimension, name: String)

  class Val[A <: AnyVal](v: A, u: UnitOfMeasure) {
    def value: A = v

    def unit: UnitOfMeasure = u
  }


  implicit def convertBaseDimensionToDimension(baseDimension: BaseDimension): Dimension = new Dimension(baseDimension)
}


object Tests {
  import UnitsOfMeasure._

  def main(args: Array[String]): Unit = {
    val m = UnitOfMeasure(Distance, "m")
    val v = new Val(5, m)
    println(v)
  }
}
