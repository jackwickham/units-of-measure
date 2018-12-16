package uk.ac.cam.jaw89.metaprogramming.units_of_measure

object BaseDimensions {

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
    * Dimensionless values (the base dimensionality)
    */
  val Dimensionless: Dimension = new Dimension(PowersOf[BaseDimension]())
}
