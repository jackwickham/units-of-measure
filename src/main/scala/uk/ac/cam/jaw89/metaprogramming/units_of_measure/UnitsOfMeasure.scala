package uk.ac.cam.jaw89.metaprogramming.units_of_measure

object UnitsOfMeasure {
  import scala.language.implicitConversions


  /**
    * For now, only allow integer powers. Non-integer (and even irrational) powers are possible, but generally rare
    */
  private type Exponent = Int
  private type PowersOf[T] = Map[T, Exponent]

  object PowersOf {
    def apply[T](elems: (T, Exponent)*): PowersOf[T] = Map.apply[T, Exponent](elems: _*)
  }


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
  case class NamedUnit[A] private[UnitsOfMeasure] (symbol: String, baseUnits: PowersOf[BaseUnit], multiplier: Option[A] = None) {
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
  case class DerivedUnit[A] private (units: PowersOf[NamedUnit[A]], multiplier: Option[A] = None) extends Dimensioned[DerivedUnit[A], NamedUnit[A]] {
    override def *(other: DerivedUnit[A]): DerivedUnit[A] = DerivedUnit(mult(other))
    override def /(other: DerivedUnit[A]): DerivedUnit[A] = DerivedUnit(div(other))
    override def ~^(power: Exponent): DerivedUnit[A] = DerivedUnit(pow(power))
    def ~^-(power: Exponent): DerivedUnit[A] = DerivedUnit(pow(-power))

    override protected def dimensionMap: PowersOf[NamedUnit[A]] = units

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
      * @param numeric A Numeric helper
      * @param power A power function that allows a multiplier to be raised to a power of type Exponent
      * @return The multiplier for this unit
      */
    def baseMultiplier(implicit nwop: Option[NumericWithOptionalPower[A, Exponent]]): Option[A] = nwop match {
      case Some(NumericWithOptionalPower(numeric, Some(power))) => Some(units.foldLeft(multiplier.getOrElse(numeric.one)) {
        case (acc, (NamedUnit(_, _, m), p)) => numeric.plus(acc, power(m.getOrElse(numeric.one), p))
      })
      case Some(NumericWithOptionalPower(numeric, None)) => Some(units.foldLeft(multiplier.getOrElse(numeric.one)) {
        case (acc, (NamedUnit(_, _, m), p)) => if (m.isEmpty || m == numeric.one || p == 1) {
          numeric.plus(acc, m.asInstanceOf[Option[A]].getOrElse(numeric.one))
        } else {
          throw new RuntimeException("An implicit power function of type (A, Exponent) => A must be provided if using non-default multipliers")
        }
      })
      case None => if (multiplier.isEmpty && units.forall(u => u._1.multiplier.isEmpty)) {
        None
      } else {
        throw new RuntimeException("An implicit Numeric[A] is required if using non-default multipliers")
      }
    }

    /**
      * Give a new name to an existing derived unit, such as `kg * m * s~^-2 alias "N"`
      *
      * @param symbol The symbol to give the new unit
      * @param numeric A Numeric helper
      * @param power A power function that allows a multiplier to be raised to the power of type Exponent
      * @return The new DerivedUnit
      */
    def alias(symbol: String)(implicit nwop: Option[NumericWithOptionalPower[A, Exponent]]): DerivedUnit[A] =
      DerivedUnit[A](PowersOf(NamedUnit[A](symbol, baseUnits) -> 1), baseMultiplier(nwop))

    private[UnitsOfMeasure] def this() = this(PowersOf[NamedUnit[A]]())
  }


  val DimensionlessUnit = new DerivedUnit[Any]()

  def defineUnit(symbol: String, dimensions: Dimension): DerivedUnit[Any] = DerivedUnit(PowersOf(NamedUnit[Any](symbol, PowersOf(BaseUnit(dimensions) -> 1)) -> 1))

  /**
    * A value with associated unit
    *
    * @param value The numeric value of this measure
    * @param unit The unit of this measure
    * @tparam A The type of the value
    */
  class Val[A](val value: A, val unit: DerivedUnit[A]) {
    def +(other: Val[A])(implicit num: Numeric[A]): Val[A] = new Val[A](num.plus(value, other.as(unit).value), unit)
    def -(other: Val[A])(implicit num: Numeric[A]): Val[A] = new Val[A](num.minus(value, other.as(unit).value), unit)
    def *(other: Val[A])(implicit num: Numeric[A]): Val[A] = new Val[A](num.times(value, other.value), unit * other.unit)
    def /(other: Val[A])(implicit num: Fractional[A]): Val[A] = new Val[A](num.div(value, other.value), unit / other.unit)
    def ~^(power: Exponent)(implicit num: Fractional[A]): Val[A] = ???

    def unary_-(implicit num: Numeric[A]): Val[A] = new Val[A](num.negate(value), unit)

    /**
      * Convert from to a different unit, with the same dimensionality
      *
      * @param targetUnit The unit to convert to
      * @return The new Val[A], scaled to the new units
      * @throws DimensionError If the unit has a different dimensionality
      */
    // TODO: Power is nasty and messes stuff up
    def as(targetUnit: DerivedUnit[A])(implicit nwop: NumericWithOptionalPower[A, Exponent]): Val[A] = if (unit == targetUnit) {
      // If units are the same, we're done
      this
    } else if (unit.baseUnits == targetUnit.baseUnits) {
      // If dimensions are the same but units are not, we need to do a bit more work
      // TODO: Division is needed
      Val(nwop.numeric.times(value, targetUnit.baseMultiplier(Some(nwop)).getOrElse(nwop.numeric.one)), targetUnit)
    } else if (unit.dimensions == targetUnit.dimensions) {
      throw UnitConversionException(unit, targetUnit)
    } else {
      throw DimensionError(unit.dimensions, targetUnit.dimensions)
    }

    override def toString: String = value.toString + " " + unit.toString

    /**
      * Do the two values represent the same measurement?
      */
    /*override def equals(obj: Any): Boolean = {
      obj match {
        case other: Val[A] => value == other.as(unit).value // TODO: wants implicits
        case _ => false
      }
    }*/

    /**
      * Are the two values exactly the same (same value and units)
      */
    def ===(other: Val[A]): Boolean = value == other.value && unit == other.unit
  }
  object Val {
    def apply[A](value: A, unit: DerivedUnit[A]): Val[A] = new Val[A](value, unit)
  }

  /**
    * General units exception
    */
  class UnitsOfMeasureException(private val message: String = "", private val cause: Throwable = None.orNull) extends RuntimeException(message, cause)
  case class DimensionError(left: Dimension, right: Dimension) extends UnitsOfMeasureException(s"Expected $left, but got $right")
  case class UnitConversionException(left: DerivedUnit[_], right: DerivedUnit[_]) extends UnitsOfMeasureException(s"Failed to convert from $left to $right - the base units are different (use alias rather than defineUnit)")

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
  implicit def convertValueToDimensionlessVal[A](v: A): Val[A] = new Val[A](v, DimensionlessUnit.asInstanceOf[DerivedUnit[A]])


  object NumericWithOptionalPower {
    implicit def fromNumericAndPower[A, E](implicit numeric: Numeric[A], power: (A, E) => A): NumericWithOptionalPower[A, E] =
      NumericWithOptionalPower(numeric, Some(power))
    implicit def fromNumeric[A, E](implicit numeric: Numeric[A]): NumericWithOptionalPower[A, E] =
      NumericWithOptionalPower(numeric, None)
  }
  case class NumericWithOptionalPower[A, E](numeric: Numeric[A], power: Option[(A, E) => A])
}


object Tests {
  import UnitsOfMeasure._

  def main(args: Array[String]): Unit = {
    val m = defineUnit("m", Length)
    val v = new Val(5, m * m)
    println(v)
  }
}
