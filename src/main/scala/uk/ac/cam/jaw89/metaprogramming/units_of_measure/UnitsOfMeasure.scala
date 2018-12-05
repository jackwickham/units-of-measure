package uk.ac.cam.jaw89.metaprogramming.units_of_measure

import scala.math.Numeric.DoubleIsFractional
import scala.math.Ordering.DoubleOrdering

object UnitsOfMeasure {
  import scala.language.implicitConversions


  /**
    * For now, only allow integer powers. Non-integer (and even irrational) powers are possible, but generally rare
    */
  private type Exponent = Int

  /**
    * Alias a term as a map from (thing being raised to the power) to Exponent
    */
  private type PowersOf[T] = Map[T, Exponent]

  object PowersOf {
    def apply[T](elems: (T, Exponent)*): PowersOf[T] = Map.apply[T, Exponent](elems: _*)
  }

  /**
    * For now, only allow unit multipliers to be defined as doubles. I can't think of a time when that won't be suitable.
    * Most multipliers will actually be powers of 10 (or 2), and we want to be able to easily multiply it by the values.
    */
  private type Multiplier = Double


  /**
    * Combine two maps, adding together keys where a value exists in both objects
    *
    * Any entries that have a result of 0 are removed
    *
    * @param x The first map
    * @param y The second map
    * @param yMultiplier A value to multiply all values of `y` by
    * @param num The object that provides `zero`, `add` and `times` for `B`
    * @tparam A The map key
    * @tparam B The map value
    * @return A map with the sum of the values for each key, and no zeros
    */
  def mapCombine[A, B](x: Map[A, B], y: Map[A, B], yMultiplier: B)(implicit num: Numeric[B]): Map[A, B] = {
    // https://gist.github.com/davidandrzej/5413350 with added generality
    val x0 = x.withDefaultValue(num.zero)
    val y0 = y.withDefaultValue(num.zero)
    val keys = x.keySet.union(y.keySet)
    keys.map{ k => k -> num.plus(x0(k), num.times(y0(k), yMultiplier))}.filter(e => e._2 != num.zero).toMap
  }

  def mapCombine[A, B](x: Map[A, B], y: Map[A, B])(implicit num: Numeric[B]): Map[A, B] = mapCombine(x, y, num.one)

  /**
    * A class that has corresponding dimensions
    *
    * @tparam A The self type -- {@see http://en.wikipedia.org/wiki/Curiously_recurring_template_pattern}
    * @tparam B The type of the dimensions
    */
  trait Dimensioned[A <: Dimensioned[A, B], B] {
    protected def mult(other: A): PowersOf[B] = mapCombine(dimensionMap, other.dimensionMap)
    protected def div(other: A): PowersOf[B] = mapCombine(dimensionMap, other.dimensionMap, -1)
    protected def pow(power: Exponent): PowersOf[B] = dimensionMap.mapValues(v => v * power)

    protected def normalise(dimensions: PowersOf[B]): PowersOf[B] = dimensions.filter(e => e._2 != 0)

    def *(other: A): A
    def /(other: A): A
    def ~^(power: Exponent): A
    protected def dimensionMap: PowersOf[B]

    override def toString: String = dimensionMap.map(e => e._1.toString + (if (e._2 == 1) "" else "^" + e._2.toString)).mkString(" ")
  }

  /**
    * A dimension (a unit's "type")
    *
    * Acceleration is represented by Dimension(Distance -> 1, Time -> -2)
    */
  class Dimension protected (_dimensions: PowersOf[BaseDimension]) extends Dimensioned[Dimension, BaseDimension] {
    val baseDimensions: PowersOf[BaseDimension] = normalise(_dimensions)

    override protected def dimensionMap: PowersOf[BaseDimension] = baseDimensions
    override def *(other: Dimension): Dimension = new Dimension(mult(other))
    override def /(other: Dimension): Dimension = new Dimension(div(other))
    override def ~^(power: Exponent): Dimension = new Dimension(pow(power))

    def this(baseDimension: BaseDimension) = this(PowersOf(baseDimension -> 1))
    private[UnitsOfMeasure] def this() = this(PowersOf[BaseDimension]())

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: Dimension => baseDimensions.equals(other.baseDimensions)
        case _ => false
      }
    }

    override def hashCode(): Int = baseDimensions.hashCode()
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
  case object Length extends BaseDimension("L")
  case object Mass extends BaseDimension("M")
  case object Time extends BaseDimension("T")
  case object Current extends BaseDimension("I")
  case object Temperature extends BaseDimension("Θ")
  case object AmountOfSubstance extends BaseDimension("N")
  case object LuminousIntensity extends BaseDimension("J")

  /**
    * A base unit, which other units are defined as multiples of, such as meters, kilograms or decibels
    *
    * Base units are constructed explicitly by the user, and by default can't be converted between (even if they have
    * the same dimensionality). This allows for different physical quantities to be given different units, even though
    * they have the same dimensions. For example, a bit and a radian are both dimensionless, but represent different
    * physical quantities, so would both be dimensionless BaseUnits.
    *
    * @param dimension The dimension that this unit is used for
    */
  case class BaseUnit private[UnitsOfMeasure] (dimension: Dimension)

  /**
    * A name that has been assigned to a base or derived unit
    *
    * NamedUnits are made up of base units and a multiplier, so a Newton is 1 * kg * m * s^-2^, and defined as
    * `NamedUnit("N", Map(kg -> 1, m -> 1, s -> -2), 1)`.
    *
    * A kilometer is defined as `NamedUnit("km", Map(m -> 1), 1000)`.
    *
    * @param symbol The symbol for this unit, such as `m`
    * @param baseUnits The base units that this name is defined in terms of
    * @param multiplier The multiplier relative to the base units
    */
  case class NamedUnit private[UnitsOfMeasure] (symbol: String, baseUnits: PowersOf[BaseUnit], multiplier: Multiplier = 1.0) {
    override def toString: String = symbol

    /**
      * Get the dimensions represented by this unit
      */
    def dimensions: Dimension = baseUnits.foldLeft(Dimensionless){
      case (acc, (BaseUnit(dimension), exp)) => acc * (dimension ~^ exp)
    }
  }

  /**
    * A unit is a particular scale of a dimension
    *
    * @param units The user-specified units that this derived unit is in terms of (this is the units displayed as the
    *              stringified version of this unit)
    * @param multiplier The ratio between this unit and the base units, or None to signify 1 (when A may be unknown)
    */
  case class DerivedUnit private (units: PowersOf[NamedUnit], multiplier: Multiplier = 1.0) extends Dimensioned[DerivedUnit, NamedUnit] {
    override def *(other: DerivedUnit): DerivedUnit = DerivedUnit(mult(other), multiplier * other.multiplier)
    override def /(other: DerivedUnit): DerivedUnit = DerivedUnit(div(other), multiplier / other.multiplier)
    override def ~^(power: Exponent): DerivedUnit = DerivedUnit(pow(power), math.pow(multiplier, power))
    def ~^-(power: Exponent): DerivedUnit = DerivedUnit(pow(-power))

    def *(mult: Multiplier): DerivedUnit = DerivedUnit(units, multiplier * mult)

    override protected def dimensionMap: PowersOf[NamedUnit] = units

    /**
      * Get the dimensions represented by this unit
      */
    def dimensions: Dimension = units.foldLeft(Dimensionless){
      case (acc, (u, exp)) => acc * (u.dimensions ~^ exp)
    }

    /**
      * Get the user-defined base units that make up this unit
      */
    def baseUnits: PowersOf[BaseUnit] = units.foldLeft(PowersOf[BaseUnit]()){
      case (acc, (u, exp)) => mapCombine(acc, u.baseUnits, exp)
    }

    /**
      * Get the multiplier of this unit compared to the base units
      *
      * @return The multiplier for this unit
      */
    def baseMultiplier: Multiplier = units.foldLeft(multiplier) {
        case (acc, (NamedUnit(_, _, m), p)) => acc * math.pow(m, p)
    }

    /**
      * Give a new name to an existing derived unit, such as `kg * m * s~^-2 alias "N"`
      *
      * @param symbol The symbol to give the new unit
      * @param numeric A Numeric helper
      * @param power A power function that allows a multiplier to be raised to the power of type Exponent
      * @return The new DerivedUnit
      */
    def alias(symbol: String): DerivedUnit = DerivedUnit(PowersOf(NamedUnit(symbol, baseUnits, baseMultiplier) -> 1))

    private[UnitsOfMeasure] def this() = this(PowersOf[NamedUnit]())

    override def toString: String = super.toString + (if (multiplier != 1.0) " * " + multiplier else "")
  }


  val DimensionlessUnit = new DerivedUnit()

  def defineUnit(symbol: String, dimensions: Dimension): DerivedUnit = DerivedUnit(PowersOf(NamedUnit(symbol, PowersOf(BaseUnit(dimensions) -> 1)) -> 1))

  /**
    * A value with associated unit
    *
    * @param value The numeric value of this measure
    * @param unit The unit of this measure
    * @tparam A The type of the value
    */
  class Measurement[A](val value: A, val unit: DerivedUnit) {
    def +(other: Measurement[A])(implicit num: MeasurementOperators[A]): Measurement[A] = new Measurement[A](num.plus(value, other.as(unit).value), unit)
    def -(other: Measurement[A])(implicit num: MeasurementOperators[A]): Measurement[A] = new Measurement[A](num.minus(value, other.as(unit).value), unit)
    def *(other: Measurement[A])(implicit num: MeasurementOperators[A]): Measurement[A] = new Measurement[A](num.times(value, other.value), unit * other.unit)
    def /(other: Measurement[A])(implicit num: MeasurementOperators[A]): Measurement[A] = new Measurement[A](num.div(value, other.value), unit / other.unit)
    def ~^(power: Exponent)(implicit num: MeasurementOperators[A]): Measurement[A] = new Measurement[A](num.pow(value, power), unit ~^ power)

    def unary_-(implicit num: Numeric[A]): Measurement[A] = new Measurement[A](num.negate(value), unit)

    /**
      * Convert from to a different unit, with the same dimensionality
      *
      * @param targetUnit The unit to convert to
      * @return The new Val[A], scaled to the new units
      * @throws DimensionError If the unit has a different dimensionality
      */
    def as(targetUnit: DerivedUnit)(implicit numeric: MeasurementOperators[A]): Measurement[A] = if (unit == targetUnit) {
      // If units are the same, we're done
      this
    } else if (unit.baseUnits == targetUnit.baseUnits) {
      // If dimensions are the same but units are not, we need to use the ratio between their multiplier
      new Measurement(numeric.div(numeric.times(value, unit.baseMultiplier), targetUnit.baseMultiplier), targetUnit)
    } else if (unit.dimensions == targetUnit.dimensions) {
      throw UnitConversionException(unit, targetUnit)
    } else {
      throw DimensionError(unit.dimensions, targetUnit.dimensions)
    }

    def in(targetUnit: DerivedUnit)(implicit numeric: MeasurementOperators[A]): Measurement[A] = as(targetUnit)(numeric)

    override def toString: String = value.toString + " " + unit.toString

    /**
      * Are the two values exactly the same (same value and units)
      */
    override def equals(obj: Any): Boolean = {
      obj match {
        case other: Measurement[A] => unit == other.unit && value == other.value
        case _ => false
      }
    }

    /**
      * Do the two values represent the same measurement?
      */
    def =~(other: Measurement[A])(implicit num: MeasurementOperators[A]): Boolean = value == other.as(unit).value
  }
  object Measurement {
    def apply[A](value: A, unit: DerivedUnit): Measurement[A] = new Measurement[A](value, unit)
  }

  /**
    * General units exception
    */
  class UnitsOfMeasureException(private val message: String = "", private val cause: Throwable = None.orNull) extends RuntimeException(message, cause)
  case class DimensionError(left: Dimension, right: Dimension) extends UnitsOfMeasureException(s"Expected $left, but got $right")
  case class UnitConversionException(left: DerivedUnit, right: DerivedUnit) extends UnitsOfMeasureException(s"Failed to convert from $left to $right - the base units are different (use alias rather than defineUnit)")

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
  implicit def convertValueToDimensionlessVal[A](v: A): Measurement[A] = new Measurement[A](v, DimensionlessUnit.asInstanceOf[DerivedUnit])

  trait MeasurementOperators[A] extends Fractional[A] {
    def times(x: A, y: Multiplier): A
    def div(x: A, y: Multiplier): A

    /**
      * Naive integer power algorithm using multiplication and division
      *
      * @param x Value to raise to a power
      * @param y Power to raise it to
      * @return x^y^
      */
    def pow(x: A, y: Exponent): A = {
      var r: A = one
      if (y < 0) {
        for (_ <- 0 until -y) {
          r = div(r, x)
        }
      } else {
        for (_ <- 0 until y) {
          r = times(r, x)
        }
      }
      r
    }
  }

  implicit object MultiplierMeasurementOperators extends DoubleIsFractional with MeasurementOperators[Multiplier] with DoubleOrdering
}


object Tests {
  import UnitsOfMeasure._

  def main(args: Array[String]): Unit = {
    val m = defineUnit("m", Length)
    val v = new Measurement(5, m * m)
    println(v)
  }
}
