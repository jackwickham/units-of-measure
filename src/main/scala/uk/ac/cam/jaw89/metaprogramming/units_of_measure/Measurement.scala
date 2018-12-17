package uk.ac.cam.jaw89.metaprogramming.units_of_measure

import scala.reflect.runtime.universe._

/**
  * A value with associated unit
  *
  * @param _value The numeric value of this measure
  * @param unit The unit of this measure
  * @tparam A The type of the value
  */
final class Measurement[A](private val _value: A, val unit: DerivedUnit)(implicit ime: IntegerMultiplyAndExponentiate[A]) {
  def +(other: Measurement[A])(implicit num: Numeric[A]): Measurement[A] =
    new Measurement[A](num.plus(_value, other.in(unit)._value), unit)
  def -(other: Measurement[A])(implicit num: Numeric[A]): Measurement[A] =
    new Measurement[A](num.minus(_value, other.in(unit)._value), unit)
  def *(other: Measurement[A])(implicit num: Numeric[A]): Measurement[A] =
    new Measurement[A](num.times(_value, other._value), unit * other.unit)
  def /(other: Measurement[A])(implicit num: Fractional[A]): Measurement[A] =
    new Measurement[A](num.div(_value, other._value), unit / other.unit)

  // Allow combining varied measurements if they have the necessary operators
  def +[B](other: Measurement[B])(implicit num: BinaryNumeric[A, B]): Measurement[A] =
    new Measurement[A](num.plus(_value, other.in(unit)._value), unit)
  def -[B](other: Measurement[B])(implicit num: BinaryNumeric[A, B]): Measurement[A] =
    new Measurement[A](num.minus(_value, other.in(unit)._value), unit)
  def *[B](other: Measurement[B])(implicit num: BinaryNumeric[A, B]): Measurement[A] =
    new Measurement[A](num.times(_value, other._value), unit * other.unit)
  def /[B](other: Measurement[B])(implicit num: BinaryFractional[A, B]): Measurement[A] =
    new Measurement[A](num.div(_value, other._value), unit / other.unit)

  def ~^(power: Exponent)(implicit num: Fractional[A]): Measurement[A] =
    new Measurement[A](ime.pow(_value, power), unit ~^ power)
  def ~^-(power: Exponent)(implicit num: Fractional[A]): Measurement[A] =
    this ~^ (-power)

  // Note: <= && >= means =~, not necessarily ==
  def <(other: Measurement[A])(implicit num: Numeric[A]): Boolean =
    num.compare(_value, other.in(unit)._value) < 0
  def <=(other: Measurement[A])(implicit num: Numeric[A]): Boolean =
    num.compare(_value, other.in(unit)._value) <= 0
  def >(other: Measurement[A])(implicit num: Numeric[A]): Boolean =
    num.compare(_value, other.in(unit)._value) > 0
  def >=(other: Measurement[A])(implicit num: Numeric[A]): Boolean =
    num.compare(_value, other.in(unit)._value) >= 0

  def unary_-(implicit num: Numeric[A]): Measurement[A] = new Measurement[A](num.negate(_value), unit)

  /**
    * Do the two values represent the same measurement?
    */
  def =~(other: Measurement[A]): Boolean = _value == other.in(unit)._value
  def !=~(other: Measurement[A]): Boolean = !(this =~ other)

  def ===(other: Measurement[A]): Boolean = unit == other.unit && _value == other._value
  def !==(other: Measurement[A]): Boolean = !(this === other)

  /**
    * Convert from to a different unit, with the same dimensionality
    *
    * @param targetUnit The unit to convert to
    * @return The new Val[A], scaled to the new units
    * @throws DimensionError If the unit has a different dimensionality
    * @throws NoUnitConversionsDefinedException If no implicit or explicit conversions to the other unit are available
    * @throws NoCompatibleConversionsException If no explicit conversions to the other unit for type A are available
    * @throws IncorrectConversionResultTypeException If an explicit conversion to the other unit was available for type
    *                                                A, but it returned something other than a subclass of A
    */
  def in(targetUnit: DerivedUnit): Measurement[A] = if (unit == targetUnit) {
    // If units are the same, we're done
    this
  } else if (unit.baseUnits == targetUnit.baseUnits) {
    // If dimensions are the same but units are not, we need to use the ratio between their multiplier
    new Measurement(ime.div(ime.times(_value, unit.baseMultiplier), targetUnit.baseMultiplier), targetUnit)
  } else if (unit.dimensions == targetUnit.dimensions) {
    try {
      // TODO: This is unchecked - no cast is performed, because everything gets erased at runtime
      new Measurement[A](unit.convert(_value, targetUnit), targetUnit)
    } catch {
      case e: ClassCastException => throw IncorrectConversionResultTypeException(this.unit, targetUnit, e)
    }
  } else {
    throw DimensionError(unit.dimensions, targetUnit.dimensions)
  }

  /**
    * Get the value in a particular unit
    *
    * @param desiredUnit The unit that the value should be in
    * @return The value, converted to the desired unit
    */
  def value(desiredUnit: DerivedUnit): A = in(desiredUnit)._value

  /**
    * Do the two values represent the same measurement, after unit conversion?
    *
    * This method returns false if the dimensions are different or the units can't be converted. If an exception is
    * desired instead, use =~, which checks that the two values are equal but throws DimensionError if the dimensions
    * are different, and UnitConversionException if the units don't have a defined conversion.
    *
    * @return Whether this measurement represents the same measure as obj
    */
  override def equals(obj: Any): Boolean = {
    obj match {
      case other: Measurement[A] => try {
          this =~ other
        } catch {
          case _: UnitsOfMeasureException => false
        }
      case _ => false
    }
  }

  override def toString: String = _value.toString + " " + unit.toString
}

object Measurement {
  import scala.language.implicitConversions

  def apply[A](value: A, unit: DerivedUnit)(implicit ime: IntegerMultiplyAndExponentiate[A]): Measurement[A] =
    new Measurement[A](value, unit)

  /**
    * Implicitly convert a raw value to a dimensionless measurement, so that it can be used
    *
    * @param v The value to convert
    * @tparam A The type of the value
    * @return A dimensionless and unitless Val[A]
    */
  implicit def convertValueToDimensionlessMeasurement[A](v: A)(implicit ime: IntegerMultiplyAndExponentiate[A]): Measurement[A] =
    new Measurement[A](v, DerivedUnit.DimensionlessUnit)

  /**
    * Implicitly convert a Numeric[A] to a `Numeric[Measurement[A]]`, with most methods implemented
    *
    * @param num Numeric[A] to convert
    * @param ime An IntegerMultiplyAndExponentiate that is needed for unit conversion
    * @tparam A The value type
    */
  implicit class NumericMeasurementAFromNumericA[A](private val num: Numeric[A])(implicit ime: IntegerMultiplyAndExponentiate[A]) extends Numeric[Measurement[A]] {
    override def compare(x: Measurement[A], y: Measurement[A]): Int = num.compare(x._value, y.in(x.unit)._value)

    override def minus(x: Measurement[A], y: Measurement[A]): Measurement[A] = x.-(y)(num)

    override def negate(x: Measurement[A]): Measurement[A] = x.unary_-(num)

    override def plus(x: Measurement[A], y: Measurement[A]): Measurement[A] = x.+(y)(num)

    override def times(x: Measurement[A], y: Measurement[A]): Measurement[A] = x.*(y)(num)

    override def toDouble(x: Measurement[A]): Double = throw new RuntimeException("Direct access to the value is not allowed - use .value instead for unit safety")
    override def toFloat(x: Measurement[A]): Float = throw new RuntimeException("Direct access to the value is not allowed - use .value instead for unit safety")
    override def toInt(x: Measurement[A]): Int = throw new RuntimeException("Direct access to the value is not allowed - use .value instead for unit safety")
    override def toLong(x: Measurement[A]): Long = throw new RuntimeException("Direct access to the value is not allowed - use .value instead for unit safety")

    override def fromInt(x: Int): Measurement[A] = new Measurement[A](num.fromInt(x), DerivedUnit.DimensionlessUnit)
  }
}
