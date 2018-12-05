package uk.ac.cam.jaw89.metaprogramming.units_of_measure


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

  /**
    * Alias of as
    */
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
  import scala.language.implicitConversions

  def apply[A](value: A, unit: DerivedUnit): Measurement[A] = new Measurement[A](value, unit)

  /**
    * Extend values to allow them to be applied to a unit, so you can do 100(m) rather than Measurement(100, m)
    */
  implicit class ValueUnitApplication[A](private val a: A) extends AnyVal {
    def apply(unit: DerivedUnit): Measurement[A] = new Measurement(a, unit)
  }

  /**
    * Implicitly convert a raw value to a dimensionless measurement, so that it can be used
    *
    * @param v The value to convert
    * @tparam A The type of the value
    * @return A dimensionless and unitless Val[A]
    */
  implicit def convertValueToDimensionlessMeasurement[A](v: A): Measurement[A] = new Measurement[A](v, DerivedUnit.DimensionlessUnit)
}
