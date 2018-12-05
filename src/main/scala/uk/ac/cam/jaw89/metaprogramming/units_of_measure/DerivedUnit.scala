package uk.ac.cam.jaw89.metaprogramming.units_of_measure

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
  def dimensions: Dimension = units.foldLeft(Dimension.Dimensionless){
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
