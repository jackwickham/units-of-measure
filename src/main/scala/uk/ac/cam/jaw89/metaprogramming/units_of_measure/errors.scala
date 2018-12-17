package uk.ac.cam.jaw89.metaprogramming.units_of_measure

/**
  * General units exception
  */
class UnitsOfMeasureException(private val message: String = "", private val cause: Throwable = null) extends RuntimeException(message, cause)

/**
  * Tried to combine two values that had different dimensions
  */
case class DimensionError(left: Dimension, right: Dimension) extends UnitsOfMeasureException(s"Expected a value with dimension $left, but got a value with dimension $right")

/**
  * Tried to implicitly convert between two units with different base units but same dimensions
  */
case class NoImplicitConversionsAvailableException(left: DerivedUnit, right: DerivedUnit) extends UnitsOfMeasureException(s"No implicit conversions are available between $left and $right (try using alias rather than definedUnit, or define an explicit conversion with defineConversion then use convertTo)")

/**
  * Tried to combine two values that had the same dimensions, but weren't compatible
  */
case class NoUnitConversionsDefinedException(left: DerivedUnit, right: DerivedUnit) extends UnitsOfMeasureException(s"No conversions available to convert from $left to $right (define one with defineConversion)")

/**
  * There's no conversion from leftUnit to rightUnit that accepts type leftType
  */
case class NoCompatibleConversionsException(leftUnit: DerivedUnit, rightUnit: DerivedUnit, leftType: String) extends
  UnitsOfMeasureException(s"Conversions are defined for converting from $leftUnit to $rightUnit, but none support converting from values of $leftType")

/**
  * The conversion from leftUnit to rightUnit produced the wrong type
  */
case class IncorrectConversionResultTypeException[A, B](leftUnit: DerivedUnit, rightUnit: DerivedUnit, wantedType: Class[A], actualType: Class[B]) extends
  UnitsOfMeasureException(s"A conversion was available when converting from $leftUnit to $rightUnit, but the result had the wrong type (wanted $wantedType but got $actualType)")
