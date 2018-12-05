package uk.ac.cam.jaw89.metaprogramming.units_of_measure

/**
  * General units exception
  */
class UnitsOfMeasureException(private val message: String = "", private val cause: Throwable = None.orNull) extends RuntimeException(message, cause)

/**
  * Tried to combine two values that had different dimensions
  */
case class DimensionError(left: Dimension, right: Dimension) extends UnitsOfMeasureException(s"Expected a value with dimension $left, but got a value with dimension $right")

/**
  * Tried to combine two values that had the same dimensions, but weren't compatible
  */
case class UnitConversionException(left: DerivedUnit, right: DerivedUnit) extends UnitsOfMeasureException(s"Failed to convert from $left to $right - the base units are different (use alias rather than defineUnit)")
