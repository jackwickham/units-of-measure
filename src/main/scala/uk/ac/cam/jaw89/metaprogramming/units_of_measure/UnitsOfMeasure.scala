package uk.ac.cam.jaw89.metaprogramming.units_of_measure

import scala.reflect.ClassTag

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
    * A class that has corresponding dimensions
    *
    * @tparam A The self type -- {@see http://en.wikipedia.org/wiki/Curiously_recurring_template_pattern}
    * @tparam B The type of the dimensions
    */
  trait Dimensioned[A <: Dimensioned[A, B], B] {
    protected def mult(other: A): Map[B, Int] = mapCombine(dimensionMap, other.dimensionMap)
    protected def div(other: A): Map[B, Int] = mapCombine(dimensionMap, other.dimensionMap, -1)
    protected def pow(power: Int): Map[B, Int] = dimensionMap.mapValues(v => v * power)

    protected def normalise(dimensions: Map[B, Int]): Map[B, Int] = dimensions.filter(e => e._2 != 0)

    def *(other: A): A
    def /(other: A): A
    def ~^(power: Int): A
    protected def dimensionMap: Map[B, Int]

    override def hashCode(): Int = dimensionMap.hashCode()

    override def toString: String = dimensionMap.map(e => e._1.toString + (if (e._2 == 1) "" else "^" + e._2.toString)).mkString(" ")
  }

  /**
    * A dimension (a unit's "type")
    *
    * Acceleration is represented by Dimension(Distance -> 1, Time -> -2)
    */
  class Dimension protected (_dimensions: Map[BaseDimension, Int]) extends Dimensioned[Dimension, BaseDimension] {
    val baseDimensions: Map[BaseDimension, Int] = normalise(_dimensions)

    override protected def dimensionMap: Map[BaseDimension, Int] = baseDimensions
    override def *(other: Dimension): Dimension = new Dimension(mult(other))
    override def /(other: Dimension): Dimension = new Dimension(div(other))
    override def ~^(power: Int): Dimension = new Dimension(pow(power))

    def this(baseDimension: BaseDimension) = this(Map(baseDimension -> 1))
    private[UnitsOfMeasure] def this() = this(Map[BaseDimension, Int]())

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: Dimension => baseDimensions.equals(other.baseDimensions)
        case _ => false
      }
    }
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
    * A user-defined unit
    *
    * @param dimension The dimensionality of this unit
    * @param symbol The symbol for this unit, such as `m`
    */
  case class DefinedUnit private (dimension: Dimension, symbol: String) {

    /**
      * A dependent type, to allow differentiating between units at a type level
      */
    object Foo

    override def toString: String = symbol
  }

  /**
    * A unit is a particular scale of a dimension
    *
    * @param dimension The dimension that this unit is for (eg hectares are Length^2^)
    * @param _baseUnits The manually user-created units that this unit is defined in terms of
    */
  case class UnitOfMeasure protected (dimension: Dimension, _baseUnits: Map[DefinedUnit, Int]) extends Dimensioned[UnitOfMeasure, DefinedUnit] {
    val baseUnits: Map[DefinedUnit, Int] = normalise(_baseUnits)

    override def *(other: UnitOfMeasure): UnitOfMeasure = new UnitOfMeasure(dimension * other.dimension, mult(other))
    override def /(other: UnitOfMeasure): UnitOfMeasure = new UnitOfMeasure(dimension / other.dimension, div(other))
    override def ~^(power: Int): UnitOfMeasure = new UnitOfMeasure(dimension ~^ 2, pow(power))

    override protected def dimensionMap: Map[DefinedUnit, Int] = baseUnits

    private[UnitsOfMeasure] def this() = this(Dimensionless, Map[DefinedUnit, Int]())

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: UnitOfMeasure => baseUnits.equals(other.baseUnits)
        case _ => false
      }
    }
  }

  object UnitOfMeasure {
    // apply creates a new DefinedUnit, and corresponding UnitOfMeasure
    def apply(dimension: Dimension, symbol: String): UnitOfMeasure = new UnitOfMeasure(dimension, Map(DefinedUnit(dimension, symbol) -> 1))
  }


  val DimensionlessUnit = new UnitOfMeasure()

  /**
    * A value with associated unit
    *
    * @param value The numeric value of this measure
    * @param unit The unit of this measure
    * @tparam A The type of the value
    */
  class Val[A](val value: A, val unit: UnitOfMeasure) {
    def +(other: Val[A])(implicit num: Numeric[A]): Val[A] = new Val[A](num.plus(value, other.as(unit).value), unit)
    def -(other: Val[A])(implicit num: Numeric[A]): Val[A] = new Val[A](num.minus(value, other.as(unit).value), unit)
    def *(other: Val[A])(implicit num: Numeric[A]): Val[A] = new Val[A](num.times(value, other.value), unit * other.unit)
    def /(other: Val[A])(implicit num: Fractional[A]): Val[A] = new Val[A](num.div(value, other.value), unit / other.unit)

    def unary_-(implicit num: Numeric[A]): Val[A] = new Val[A](num.negate(value), unit)

    /**
      * Convert from to a different unit, with the same dimensionality
      *
      * @param targetUnit The unit to convert to
      * @return The new Val[A], scaled to the new units
      * @throws DimensionError If the unit has a different dimensionality
      */
    def as(targetUnit: UnitOfMeasure): Val[A] = if (unit == targetUnit) {
      // If units are the same, we're done
      this
    } else if (unit.dimension == targetUnit.dimension) {
      // If dimensions are the same but units are not, we need to do a bit more work
      ???
    } else {
      throw DimensionError(unit.dimension, targetUnit.dimension)
    }

    override def toString: String = value.toString + " " + unit.toString

    /**
      * Do the two values represent the same measurement?
      */
    override def equals(obj: Any): Boolean = {
      obj match {
        case other: Val[_] => value == other.as(unit).value
        case _ => false
      }
    }

    /**
      * Are the two values exactly the same (same value and units)
      */
    def ===(other: Val[A]): Boolean = value == other.value && unit == other.unit
  }
  object Val {
    def apply[A](value: A, unit: UnitOfMeasure): Val[A] = new Val[A](value, unit)
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
    *
    * @param v The value to convert
    * @tparam A The type of the value
    * @return A dimensionless and unitless Val[A]
    */
  implicit def convertValueToDimensionlessVal[A](v: A): Val[A] = new Val[A](v, DimensionlessUnit)
}


object Tests {
  import UnitsOfMeasure._

  def main(args: Array[String]): Unit = {
    val m = UnitOfMeasure(Length, "m")
    val v = new Val(5, m * m)
    println(v)
  }
}
