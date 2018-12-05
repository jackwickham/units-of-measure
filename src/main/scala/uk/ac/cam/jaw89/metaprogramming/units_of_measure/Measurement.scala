package uk.ac.cam.jaw89.metaprogramming.units_of_measure

/**
  * A value with associated unit
  *
  * @param _value The numeric value of this measure
  * @param unit The unit of this measure
  * @tparam A The type of the value
  */
final class Measurement[A](private val _value: A, val unit: DerivedUnit) {
  def +(other: Measurement[A])(implicit num: Numeric[A], ime: IntegerMultiplyAndExponentiate[A]): Measurement[A] =
    new Measurement[A](num.plus(_value, other.as(unit)._value), unit)
  def -(other: Measurement[A])(implicit num: Numeric[A], ime: IntegerMultiplyAndExponentiate[A]): Measurement[A] =
    new Measurement[A](num.minus(_value, other.as(unit)._value), unit)
  def *(other: Measurement[A])(implicit num: Numeric[A]): Measurement[A] =
    new Measurement[A](num.times(_value, other._value), unit * other.unit)
  def /(other: Measurement[A])(implicit num: Fractional[A]): Measurement[A] =
    new Measurement[A](num.div(_value, other._value), unit / other.unit)

  def ~^(power: Exponent)(implicit num: IntegerMultiplyAndExponentiate[A] with Fractional[A]): Measurement[A] =
    new Measurement[A](num.pow(_value, power), unit ~^ power)
  def ~^-(power: Exponent)(implicit num: IntegerMultiplyAndExponentiate[A] with Fractional[A]): Measurement[A] =
    this ~^ (-power)

  // Note: <= && >= means =~, not necessarily ==
  def <(other: Measurement[A])(implicit num: IntegerMultiplyAndExponentiate[A] with Numeric[A]): Boolean =
    num.compare(_value, other.as(unit)._value) < 0
  def <=(other: Measurement[A])(implicit num: IntegerMultiplyAndExponentiate[A] with Numeric[A]): Boolean =
    num.compare(_value, other.as(unit)._value) <= 0
  def >(other: Measurement[A])(implicit num: IntegerMultiplyAndExponentiate[A] with Numeric[A]): Boolean =
    num.compare(_value, other.as(unit)._value) > 0
  def >=(other: Measurement[A])(implicit num: IntegerMultiplyAndExponentiate[A] with Numeric[A]): Boolean =
    num.compare(_value, other.as(unit)._value) >= 0

  def unary_-(implicit num: Numeric[A]): Measurement[A] = new Measurement[A](num.negate(_value), unit)

  /**
    * Do the two values represent the same measurement?
    */
  def =~(other: Measurement[A])(implicit num: IntegerMultiplyAndExponentiate[A]): Boolean = _value == other.as(unit)._value
  def !=~(other: Measurement[A])(implicit num: IntegerMultiplyAndExponentiate[A]): Boolean = !(this =~ other)

  /**
    * Convert from to a different unit, with the same dimensionality
    *
    * @param targetUnit The unit to convert to
    * @return The new Val[A], scaled to the new units
    * @throws DimensionError If the unit has a different dimensionality
    */
  def as(targetUnit: DerivedUnit)(implicit numeric: IntegerMultiplyAndExponentiate[A]): Measurement[A] = if (unit == targetUnit) {
    // If units are the same, we're done
    this
  } else if (unit.baseUnits == targetUnit.baseUnits) {
    // If dimensions are the same but units are not, we need to use the ratio between their multiplier
    new Measurement(numeric.div(numeric.times(_value, unit.baseMultiplier), targetUnit.baseMultiplier), targetUnit)
  } else if (unit.dimensions == targetUnit.dimensions) {
    throw UnitConversionException(unit, targetUnit)
  } else {
    throw DimensionError(unit.dimensions, targetUnit.dimensions)
  }

  /**
    * Alias of as
    */
  def in(targetUnit: DerivedUnit)(implicit numeric: IntegerMultiplyAndExponentiate[A]): Measurement[A] = as(targetUnit)(numeric)

  /**
    * Get the value in a particular unit
    *
    * @param desiredUnit The unit that the value should be in
    * @param numeric Measurement operators to use to calculate the values
    * @return The value, converted to the desired unit
    */
  def value(desiredUnit: DerivedUnit)(implicit numeric: IntegerMultiplyAndExponentiate[A]): A = as(desiredUnit)(numeric)._value

  /**
    * Are the two values exactly the same (same value and units)
    */
  override def equals(obj: Any): Boolean = {
    obj match {
      case other: Measurement[A] => unit == other.unit && _value == other._value
      case _ => false
    }
  }

  override def toString: String = _value.toString + " " + unit.toString
}

object Measurement {
  import scala.language.implicitConversions

  def apply[A](value: A, unit: DerivedUnit): Measurement[A] = new Measurement[A](value, unit)

  /**
    * Implicitly convert a raw value to a dimensionless measurement, so that it can be used
    *
    * @param v The value to convert
    * @tparam A The type of the value
    * @return A dimensionless and unitless Val[A]
    */
  implicit def convertValueToDimensionlessMeasurement[A](v: A): Measurement[A] = new Measurement[A](v, DerivedUnit.DimensionlessUnit)

  /**
    * Implicitly convert a Numeric[A] to a `Numeric[Measurement[A]]`, with most methods implemented
    *
    * @param num Numeric[A] to convert
    * @param ime An IntegerMultiplyAndExponentiate that is needed for unit conversion
    * @tparam A The value type
    */
  implicit class NumericMeasurementAFromNumericA[A](private val num: Numeric[A])(implicit ime: IntegerMultiplyAndExponentiate[A]) extends Numeric[Measurement[A]] {
    override def compare(x: Measurement[A], y: Measurement[A]): Int = num.compare(x._value, y.as(x.unit)._value)

    override def minus(x: Measurement[A], y: Measurement[A]): Measurement[A] = x.-(y)(num, ime)

    override def negate(x: Measurement[A]): Measurement[A] = x.unary_-(num)

    override def plus(x: Measurement[A], y: Measurement[A]): Measurement[A] = x.+(y)(num, ime)

    override def times(x: Measurement[A], y: Measurement[A]): Measurement[A] = x.*(y)(num)

    override def toDouble(x: Measurement[A]): Double = throw new RuntimeException("Direct access to the value is not allowed - use .value instead for unit safety")
    override def toFloat(x: Measurement[A]): Float = throw new RuntimeException("Direct access to the value is not allowed - use .value instead for unit safety")
    override def toInt(x: Measurement[A]): Int = throw new RuntimeException("Direct access to the value is not allowed - use .value instead for unit safety")
    override def toLong(x: Measurement[A]): Long = throw new RuntimeException("Direct access to the value is not allowed - use .value instead for unit safety")

    override def fromInt(x: Int): Measurement[A] = new Measurement[A](num.fromInt(x), DerivedUnit.DimensionlessUnit)
  }
}
