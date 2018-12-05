package uk.ac.cam.jaw89.metaprogramming.units_of_measure

/**
  * A base unit, which other units are defined as multiples of, such as meters, kilograms or decibels
  *
  * Base units are constructed explicitly by the user, and by default can't be converted between (even if they have
  * the same dimensionality). This allows for different physical quantities to be given different units, even though
  * they have the same dimensions. For example, a bit and a radian are both dimensionless, but represent different
  * physical quantities, so would both be dimensionless BaseUnits.
  *
  * BaseUnits don't have names (though they are always associated with a NamedUnit that does), and they have a unique
  * identity (so they are compared using reference equality)
  *
  * @param dimension The dimension that this unit is used for
  */
class BaseUnit (val dimension: Dimension)
