package uk.ac.cam.jaw89.metaprogramming.units_of_measure

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * A unit is a particular scale of a dimension
  *
  * @param units The user-specified units that this derived unit is in terms of (this is the units displayed as the
  *              stringified version of this unit)
  * @param multiplier The ratio between this unit and the base units, or None to signify 1 (when A may be unknown)
  */
final case class DerivedUnit private (units: PowersOf[NamedUnit], multiplier: Multiplier = 1.0) extends Dimensioned[DerivedUnit, NamedUnit] {
  override def *(other: DerivedUnit): DerivedUnit = DerivedUnit(mult(other), multiplier * other.multiplier)
  override def /(other: DerivedUnit): DerivedUnit = DerivedUnit(div(other), multiplier / other.multiplier)
  override def ~^(power: Exponent): DerivedUnit = DerivedUnit(pow(power), math.pow(multiplier, power))
  def ~^-(power: Exponent): DerivedUnit = DerivedUnit(pow(-power))

  override protected def dimensionMap: PowersOf[NamedUnit] = units

  /**
    * Multiplying a unit by a Multiplier (=Double) creates a new unit with the increased multiplier. For example,
    * minute = second * 60
    *
    * For pre-multiplication, see DerivedUnit#PrefixMultiplier
    *
    * @param mult The value being multiplied by
    * @return A new unit with an increased multiplier
    */
  def *(mult: Multiplier): DerivedUnit = DerivedUnit(units, multiplier * mult)

  /**
    * Get the dimensions represented by this unit
    */
  def dimensions: Dimension = units.foldLeft(BaseDimensions.Dimensionless){
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
    case (acc, (u, p)) => acc * math.pow(u.multiplier, p)
  }

  /**
    * Give a new name to an existing derived unit, such as `kg * m * s~^-2 alias "N"`
    *
    * @param symbol The symbol to give the new unit
    * @return The new DerivedUnit
    */
  def alias(symbol: String): DerivedUnit = DerivedUnit(PowersOf(new NamedUnit(symbol, baseUnits, baseMultiplier) -> 1))

  /**
    * A set of defined conversions for this unit
    *
    * Map from destination unit to [Map from input type to conversion function]
    */
  private val definedConversions: mutable.Map[DerivedUnit, mutable.Map[Class[_], _ => _]] = mutable.Map()

  /**
    * Define a conversion from this unit to `to`
    *
    * The units must have the same dimensions
    *
    * @param to The unit to convert to
    * @param convert The conversion function, that takes a value of type ValueType which is a measurement in this unit,
    *                and returns a value of type ResultType which is a measurement in unit to
    * @param valueTag A Class for the type of the value being converted from
    * @tparam ValueType The type being converted from
    * @tparam ResultType The type being converted to
    * @throws DimensionError if the units have different dimensions
    */
  def defineConversion[ValueType, BoxedValueType <: AnyRef, ResultType]
                      (to: DerivedUnit, convert: ValueType => ResultType)
                      (implicit conv: ValueType => BoxedValueType, valueTag: ClassTag[BoxedValueType]): Unit = {
    if (dimensions != to.dimensions) {
      throw DimensionError(dimensions, to.dimensions)
    }
    if (this.eq(to)) {
      throw new RuntimeException("Can't define a conversion from a unit to itself")
    }
    definedConversions.getOrElseUpdate(to, mutable.Map()) += (valueTag.runtimeClass -> convert)
    ()
  }

  /**
    * Convert `value` from this unit to `to`
    *
    * An explicit conversion must have been registered in advance to use this method. Most of the time, Measurement.in
    * is more suitable, because it also performs implicit conversions too.
    *
    * The result type often needs to be explicitly specified, because the compiler often infers None rather than Any.
    * Note that a ClassCastException may be thrown when storing the result if ResultType isn't the correct result type.
    * Due to type erasure, this can't be checked within the method.
    *
    * @param value The value to convert
    * @param to The DerivedUnit to convert to
    * @tparam ValueType The type of value
    * @tparam ResultType The return type
    * @return The converted value
    * @throws DimensionError if this and to have different dimensions, and are therefore not possible to convert between
    * @throws NoUnitConversionsDefinedException if no conversions have been defined between this and to
    * @throws NoCompatibleConversionsException if a conversion between this and to exists, but none has been declared
    *                                          that converts from ValueType to any type
    */
  def convert[ValueType, ResultType](value: ValueType, to: DerivedUnit): ResultType = {
    val availableConversions = definedConversions.get(to) match {
      case Some(conversions) => conversions
      case None => // No defined conversion - check if a conversion is even possible
        if (dimensions == to.dimensions) {
          throw NoUnitConversionsDefinedException(this, to)
        } else {
          throw DimensionError(dimensions, to.dimensions)
        }
    }
    availableConversions.get(value.getClass) match {
      case Some(conversion) => conversion.asInstanceOf[ValueType => ResultType](value)
      case None => throw NoCompatibleConversionsException(this, to, value.getClass.getName)
    }
  }

  override def toString: String = super.toString + (if (multiplier != 1.0) " * " + multiplier else "")
}

object DerivedUnit {
  val DimensionlessUnit = new DerivedUnit(PowersOf[NamedUnit]())

  /**
    * Define a new base unit, and build a derived unit from it
    *
    * @param symbol The symbol for this unit, such as m or kg
    * @param dimensions The dimensionality of this unit
    * @return A new DerivedUnit representing 1 of the new unit
    */
  def defineUnit(symbol: String, dimensions: Dimension): DerivedUnit = new DerivedUnit(PowersOf(
    // Create a new NamedUnit, with the symbol for this new unit
    new NamedUnit(symbol, PowersOf(
      // The NamedUnit represents a newly created unit, which has no relationship to any other unit, so create a new
      // BaseUnit here with the desired dimensionality
      new BaseUnit(dimensions) -> 1
    )) -> 1
  ))


  /**
    * Extend Multiplier (= Double) so that it can be pre-multiplied by a unit to produce a new unit, such as
    * minute = 60 * second
    */
  implicit class PrefixMultiplier(private val a: Multiplier) extends AnyVal {
    def *(original: DerivedUnit): DerivedUnit = DerivedUnit(original.units, a * original.multiplier)
  }
}
