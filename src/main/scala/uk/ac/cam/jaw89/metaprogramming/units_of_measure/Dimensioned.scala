package uk.ac.cam.jaw89.metaprogramming.units_of_measure

/**
  * A class that has corresponding dimensions
  *
  * @tparam A The self type -- {@see http://en.wikipedia.org/wiki/Curiously_recurring_template_pattern}
  * @tparam B The type of the dimensions
  */
trait Dimensioned[A <: Dimensioned[A, B], B] {
  protected def mult(other: A): PowersOf[B] = mapCombine(dimensionMap, other.dimensionMap)
  protected def div(other: A): PowersOf[B] = mapCombine(dimensionMap, other.dimensionMap, -1)
  protected def pow(power: Exponent): PowersOf[B] = dimensionMap.mapValues(v => v * power)

  protected def normalise(dimensions: PowersOf[B]): PowersOf[B] = dimensions.filter(e => e._2 != 0)

  def *(other: A): A
  def /(other: A): A
  def ~^(power: Exponent): A
  protected def dimensionMap: PowersOf[B]

  override def toString: String = dimensionMap.map(e => e._1.toString + (if (e._2 == 1) "" else "^" + e._2.toString)).mkString(" ")
}
