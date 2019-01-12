package uk.ac.cam.jaw89.metaprogramming.units_of_measure

/**
  * A name that has been assigned to a base or derived unit
  *
  * NamedUnits are made up of base units and a multiplier, so a Newton is 1 * kg * m * s^-2^, and defined as
  * `NamedUnit("N", Map(kg -> 1, m -> 1, s -> -2), 1)`.
  *
  * A kilometer is defined as `NamedUnit("km", PowersOf(m -> 1), 1000)`.
  *
  * @param symbol The symbol for this unit, such as `m`
  * @param baseUnits The base units that this name is defined in terms of
  * @param multiplier The multiplier relative to the base units
  */
class NamedUnit private[units_of_measure] (val symbol: String,
                                           private[units_of_measure] val baseUnits: PowersOf[BaseUnit],
                                           private[units_of_measure] val multiplier: Multiplier = 1.0) {

  override def toString: String = symbol

  /**
    * Get the dimensions represented by this unit
    */
  lazy val dimensions: Dimension = baseUnits.foldLeft(BaseDimensions.Dimensionless){
    case (acc, (baseUnit, exp)) => acc * (baseUnit.dimension ~^ exp)
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: NamedUnit => symbol == other.symbol && baseUnits == other.baseUnits && multiplier == other.multiplier
    case _ => false
  }

  override def hashCode(): Int = symbol.hashCode ^ baseUnits.hashCode() ^ multiplier.hashCode()
}
