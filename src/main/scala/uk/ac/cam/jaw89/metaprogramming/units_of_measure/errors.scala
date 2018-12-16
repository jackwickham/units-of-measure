package uk.ac.cam.jaw89.metaprogramming.units_of_measure

/**
  * General units exception
  */
abstract class UnitsOfMeasureException(private val message: String = "", private val cause: Throwable = None.orNull) extends RuntimeException(message, cause)

/**
  * Tried to combine two values that had different dimensions
  */
case class DimensionError(left: Dimension, right: Dimension) extends UnitsOfMeasureException(s"Expected a value with dimension $left, but got a value with dimension $right")

/**
  * Tried to combine two values that had the same dimensions, but weren't compatible
  */
case class NoUnitConversionsDefinedException(left: DerivedUnit, right: DerivedUnit) extends UnitsOfMeasureException(s"No conversions available to convert from $left to $right (try using alias rather than defineUnit, or use defineConversion)")

case class NoCompatibleConversionsException(leftUnit: DerivedUnit, rightUnit: DerivedUnit, leftType: String)
  extends UnitsOfMeasureException(s"Conversions are defined for converting from $leftUnit to $rightUnit, but none support converting from values of $leftType")
