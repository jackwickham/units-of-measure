package uk.ac.cam.jaw89.metaprogramming

package object units_of_measure {
  import scala.language.implicitConversions


  /**
    * For now, only allow integer powers. Non-integer (and even irrational) powers are possible, but generally rare
    */
  private[units_of_measure] type Exponent = Int

  /**
    * Alias a term as a map from (thing being raised to the power) to Exponent
    */
  private[units_of_measure] type PowersOf[T] = Map[T, Exponent]

  private[units_of_measure] object PowersOf {
    /**
      * Create a new PowersOf map
      *
      * @see Map#apply
      */
    def apply[T](elems: (T, Exponent)*): PowersOf[T] = Map.apply[T, Exponent](elems: _*)
  }

  /**
    * For now, only allow unit multipliers to be defined as doubles. I can't think of a time when that won't be suitable.
    * Most multipliers will actually be powers of 10 (or 2), and we want to be able to easily multiply it by the values.
    */
  private[units_of_measure] type Multiplier = Double

  /**
    * Combine two maps, adding together keys where a value exists in both objects
    *
    * Any entries that have a result of 0 are removed
    *
    * @param x The first map
    * @param y The second map
    * @param yMultiplier A value to multiply all values of `y` by
    * @param num The object that provides `zero`, `add` and `times` for `B`
    * @tparam A The map key
    * @tparam B The map value
    * @return A map with the sum of the values for each key, and no zeros
    */
  private[units_of_measure] def mapCombine[A, B](x: Map[A, B], y: Map[A, B], yMultiplier: B)(implicit num: Numeric[B]): Map[A, B] = {
    // https://gist.github.com/davidandrzej/5413350 with added generality
    val x0 = x.withDefaultValue(num.zero)
    val y0 = y.withDefaultValue(num.zero)
    val keys = x.keySet.union(y.keySet)
    keys.map{ k => k -> num.plus(x0(k), num.times(y0(k), yMultiplier))}.filter(e => e._2 != num.zero).toMap
  }

  /**
    * Convenience method which defaults to multiplying by 1
    */
  private[units_of_measure] def mapCombine[A, B](x: Map[A, B], y: Map[A, B])(implicit num: Numeric[B]): Map[A, B] = mapCombine(x, y, num.one)


  /**
    * Extend values to allow them to be applied to a unit, so you can do 100(m) rather than Measurement(100, m)
    */
  implicit class ValueUnitApplication[A](private val a: A) extends AnyVal {
    def apply(unit: DerivedUnit)(implicit ime: MultiplyAndExponentiate[A]): Measurement[A] = new Measurement(a, unit)
  }


  //// Re-export some useful methods ////

  /**
    * Define a new unit, which isn't related to any previously defined units
    *
    * @param symbol The symbol for this unit, such as m or kg
    * @param dimensions The dimensionality of this unit
    * @return A new DerivedUnit representing 1 of the new unit
    */
  def defineUnit(symbol: String, dimensions: Dimension): DerivedUnit = DerivedUnit.defineUnit(symbol, dimensions)
}
