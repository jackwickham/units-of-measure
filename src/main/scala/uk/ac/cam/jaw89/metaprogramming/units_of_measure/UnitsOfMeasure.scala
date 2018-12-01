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
  class Dimension private[UnitsOfMeasure] (dimensions: Map[BaseDimension, Int]) {
    val baseDimensions: Map[BaseDimension, Int] = normalise(dimensions)

    def *(other: Dimension): Dimension = new Dimension(mapCombine(baseDimensions, other.baseDimensions))
    def /(other: Dimension): Dimension = new Dimension(mapCombine(baseDimensions, other.baseDimensions, -1))
    def ~^(power: Int): Dimension = new Dimension(baseDimensions.mapValues(v => v * power))

    private def normalise(dimensions: Map[BaseDimension, Int]): Map[BaseDimension, Int] = dimensions.filter(e => e._2 != 0)

    def this(baseDimension: BaseDimension) = this(Map(baseDimension -> 1))
    private[UnitsOfMeasure] def this() = this(Map[BaseDimension, Int]())

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: Dimension => baseDimensions.equals(other.baseDimensions)
        case _ => false
      }
    }

    override def hashCode(): Int = baseDimensions.hashCode()

    override def toString: String = dimensions.map(e => e._1.toString + (if (e._2 == 1) "" else "^" + e._2.toString)).mkString(" ")
  }

  /**
    * Dimensionless values
    */
  val Dimensionless: Dimension = new Dimension()

  /**
    * The base dimensions, as defined by SI
    */
  sealed abstract class BaseDimension(val name: String) {
    override def toString: String = name
  }
  case object Length extends BaseDimension("Length")
  case object Mass extends BaseDimension("Mass")
  case object Time extends BaseDimension("Time")
  case object Current extends BaseDimension("Current")
  case object Temperature extends BaseDimension("Temperature")
  case object AmountOfSubstance extends BaseDimension("AmountOfSubstance")
  case object LuminousIntensity extends BaseDimension("LuminousIntensity")

  /**
    * A unit is a particular scale of a dimension
    *
    * @param dimension The dimension that this unit is for (eg hectares are Length^2^)
    * @param name The display name for this variable
    * @param scale The scale of this unit compared to the SI base unit for the dimension (or combination of base units
    *              for composite dimensions)
    */
  case class UnitOfMeasure(dimension: Dimension, name: String, scale: Double) {
    override def toString: String = name

    def *(other: UnitOfMeasure): UnitOfMeasure = new UnitOfMeasure(dimension * other.dimension, name + " " + other.name, scale * other.scale)
    def /(other: UnitOfMeasure): UnitOfMeasure = new UnitOfMeasure(dimension / other.dimension, name + "/" + other.name, scale / other.scale)
    def ~^(power: Integer): UnitOfMeasure = new UnitOfMeasure(dimension ~^ power, s"($name)^$power", scala.math.pow(scale, power.toDouble))

    def apply(other: UnitOfMeasure): UnitOfMeasure = this * other
  }


  val DimensionlessUnit = new UnitOfMeasure(Dimensionless, "", 1)

  /**
    * A value with associated unit
    * @param value
    * @param unit
    * @tparam A
    */
  class Val[A](val value: A, val unit: UnitOfMeasure) {
    def +(other: Val[A])(implicit num: Numeric[A]): Val[A] = if (unit == other.unit) {
      // If units are the same, we're done
      new Val[A](num.plus(value, other.value), unit)
    } else if (unit.dimension == other.unit.dimension) {
      // If dimensions are the same but units are not, we need to do a bit more work
      ???
    } else {
      throw DimensionError(unit.dimension, other.unit.dimension)
    }

    override def toString: String = value.toString + " " + unit.toString
  }

  /**
    * General units exception
    */
  class UnitsOfMeasureException(private val message: String = "", private val cause: Throwable = None.orNull) extends RuntimeException(message, cause)
  case class DimensionError(left: Dimension, right: Dimension) extends UnitsOfMeasureException(s"Expected $left, but got $right")
  case class UnitConversionException(left: UnitOfMeasure, right: UnitOfMeasure) extends UnitsOfMeasureException(s"Failed to convert from $left to $right")

  /**
    * Implicitly convert BaseDimension to Dimension, to make it easier to use
    */
  implicit def convertBaseDimensionToDimension(baseDimension: BaseDimension): Dimension = new Dimension(baseDimension)

  /**
    * Implicitly convert a raw value to a dimensionless value, so that it can be used
    * @param v
    * @tparam A
    * @return
    */
  implicit def convertValueToDimensionlessVal[A](v: A): Val[A] = new Val[A](v, DimensionlessUnit)
}


object Tests {
  import UnitsOfMeasure._

  def main(args: Array[String]): Unit = {
    val m = UnitOfMeasure(Length, "m", 1)
    val v = new Val(5, m(m))
    println(v)
  }
}
