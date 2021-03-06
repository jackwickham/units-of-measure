package uk.ac.cam.jaw89.metaprogramming.units_of_measure

import uk.ac.cam.jaw89.metaprogramming.units_of_measure.BaseDimensions.BaseDimension

/**
  * A dimension (a unit's "type")
  *
  * Acceleration is represented by Dimension(Distance -> 1, Time -> -2)
  */
class Dimension private[units_of_measure] (_dimensions: PowersOf[BaseDimension]) extends Dimensioned[Dimension, BaseDimension] {
  val baseDimensions: PowersOf[BaseDimension] = normalise(_dimensions)

  override protected def dimensionMap: PowersOf[BaseDimension] = baseDimensions
  override def *(other: Dimension): Dimension = new Dimension(mult(other))
  override def /(other: Dimension): Dimension = new Dimension(div(other))
  override def ~^(power: Exponent): Dimension = new Dimension(pow(power))
  def ~^-(power: Exponent): Dimension = new Dimension(pow(-power))

  def this(baseDimension: BaseDimension) = this(PowersOf(baseDimension -> 1))

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: Dimension => baseDimensions.equals(other.baseDimensions)
      case _ => false
    }
  }

  override def hashCode(): Int = baseDimensions.hashCode()
}
