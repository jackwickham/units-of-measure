package uk.ac.cam.jaw89.metaprogramming.units_of_measure

trait Dsl {
  import scala.language.higherKinds

  type Dimension
  type UnitOfMeasure
  type Value[_]

  def Length: Dimension
  def Mass: Dimension
  def Time: Dimension
  def Current: Dimension
  def Temperature: Dimension
  def AmountOfSubstance: Dimension
  def LuminousIntensity: Dimension


}
