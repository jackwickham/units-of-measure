package uk.ac.cam.jaw89.metaprogramming.units_of_measure

import uk.ac.cam.jaw89.metaprogramming.units_of_measure.BaseDimensions._

/**
  * Base SI units, as defined in https://en.wikipedia.org/wiki/International_System_of_Units
  */
object SI {
  val m: DerivedUnit = defineUnit("m", Length)
  val kg: DerivedUnit = defineUnit("kg", Mass)
  val s: DerivedUnit = defineUnit("s", Time)
  val A: DerivedUnit = defineUnit("A", Current)
  val K: DerivedUnit = defineUnit("K", Temperature)
  val mol: DerivedUnit = defineUnit("mol", AmountOfSubstance)
  val cd: DerivedUnit = defineUnit("cd", LuminousIntensity)

  /**
    * SI derived units, as defined in https://en.wikipedia.org/wiki/SI_derived_unit
    */
  object Derived {
    val Hz: DerivedUnit = s~^-1 alias "Hz"
    val rad: DerivedUnit = defineUnit("rad", Dimensionless)
    val sr: DerivedUnit = defineUnit("sr", Dimensionless)
    val N: DerivedUnit = kg * m * s~^-2 alias "N"
    val Pa: DerivedUnit = kg * m~^-1 * s~^-2 alias "Pa"
    val J: DerivedUnit = kg * m~^2 * s~^-2 alias "J"
    val W: DerivedUnit = kg * m~^2 * s~^-3 alias "W"
    val C: DerivedUnit = s * A alias "C"
    val V: DerivedUnit = kg * m~^2 * s~^-3 * A~^-1 alias "V"
    val F: DerivedUnit = kg~^-1 * m~^-2 * s~^4 * A~^2 alias "F"
    val Ω: DerivedUnit = kg * m~^2 * s~^-3 * A~^-2 alias "Ω"
    val ohm: DerivedUnit = Ω
    val S: DerivedUnit = Ω~^-1 alias "S"
    val Wb: DerivedUnit = J / A alias "Wb"
    val T: DerivedUnit = V * s * m~^-2 alias "T"
    val H: DerivedUnit = Ω * s alias "H"
    val celsius: DerivedUnit = defineUnit("°C", Temperature)
    val degC: DerivedUnit = celsius
    val lm: DerivedUnit = cd * sr alias "lm"
    val lux: DerivedUnit = lm * m~^-2 alias "lux"
    val Bq: DerivedUnit = s~^-1 alias "Bq"
    val Gy: DerivedUnit = J / kg alias "Gy"
    val Sv: DerivedUnit = J / kg alias "Sv"
    val kat: DerivedUnit = mol / s alias "kat"

    celsius.defineConversion(K, (c: Double) => c + 273.15)
    K.defineConversion(celsius, (k: Double) => k - 273.15)
  }
}
