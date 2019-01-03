# Units of Measure

## Usage
To import the core library, including all of the DSL syntactic sugar, start with
```scala
import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
```

### Units
Units can be defined using `defineUnit`. The dimensions of the unit, in the SI base units, are provided to allow dimensional analysis to be performed.
```scala
val mph = defineUnit("mph", BaseDimensions.Length / BaseDimensions.Time)
val watt = defineUnit("W", BaseDimensions.Mass * BaseDimensions.Length~^2
                           * BaseDimensions.Time~^-3)
val degree = defineUnit("°", BaseDimensions.Dimensionless)
```
Alternatively, the SI base units (metre, kilogram, second, ampere, kelvin, mole and candela) can be imported from `SI`, and the standard SI derived units can be imported from `SI.Derived`.

A unit can also be defined as combination of other units, with an optional multiplier. The `alias` operator can be used to give the new unit a name, which is used when a measurement is stringified.
```scala
val metresPerSecond = SI.m / SI.s
val ft = 0.305 * SI.m alias "ft"
val newtons = SI.kg * SI.m / SI.s~^2 alias "N"
```

### Measurements
A measurement is a value with an associated unit. Measurements are usually created by applying the unit to the value, but in expressions it is sometimes necessary to use the `Measurement` constructor to avoid ambiguity about whether it is an implicit argument or a measurement.
```scala
val earthCircumference = Measurement(40075000.0, SI.m)
val c = 299792458.0(SI.m / SI.s)
```

The value of a `Measurement[A]` is polymorphic, allowing it to be used with a range of different value types. Any value type must provide an implicit `MultiplyAndExponentiate[A]`, which provides multiplication and division by `Double`, to support implicit conversion to other units, and exponentiation by `Int`. An implementation is provided for `Double`.

Two measurements can be combined with `*` and `/`, producing a measurement with the combination of their units. If the units of the operands are implicitly inter-convertible, `+` and `-` can be used too, and the result is a measurement with the same units as the left operand.

If the value type is the same for the two operands, an implicit `Numeric[A]` or `Fractional[A]` is required; if the value type differs, an implicit `BinaryNumeric[A, B]` or `BinaryFractional[A, B]` is required. Scalar multiplication of vectors is an example where heterogeneous operations are needed.

Measurements can also be raised to integer powers, using the `~^` operator (to provide the operator precedence of exponentiation).

```scala
val distance = 100.0(SI.m)
val time = 9.58(SI.s)
val velocity = distance / time // 10.44 m s^-1
val acceleration = distance / time~^2 // 1.09 m s^-2
```

Attempting to add or subtract incompatible units will result in a `UnitsOfMeasureException`. If the dimensions of the values match, this is specialised to `NoImplicitConversionsAvailableException`, and if they differ it is specialised to `DimensionError`. A `DimensionError` usually represents a mistake in the program logic, while a `NoImplicitConversionsAvailableException` can usually be fixed by adding an explicit conversion or manually converting the values.
```scala
30.0(SI.K) + 10.2(SI.Derived.celcius) // NoImplicitConversionsAvailableException
28.7(SI.m) - 5.1(SI.s) // DimensionError
```

### Unit Conversions
Two units can be implicitly converted between if, when tracing their definitions back, they share the same base set of units that were defined using `defineUnit`. Implicit conversions are performed during addition and subtraction, and when calling `in` or `value` with a compatible unit.
```scala
val distance = 10.5(SI.m) + 2.0(ft) // (10.5 + 2.0 * 0.305) m = 11.11 m
```

If the base units differ, an explicit conversion can be defined, which provides a function for performing the conversion. Explicit conversions are one-way, and are only used when `convertTo` is called.
```scala
val degC = defineUnit("°C", BaseDimensions.Temperature)
SI.K.defineConversion(degC, (c: Double) => c - 273.15)
val temp = 300.0(SI.K).convertTo(degC) // 26.85 °C
-12.5(degC).convertTo(SI.K) // NoUnitConversionsDefinedException
15.0(degC) + 12.4(SI.K) // NoImplicitConversionsAvailableException
```
Explicit conversions can only be defined if the dimensions of the two units have the same dimensions. Conversions that involve multiplying by a value that has dimension have to be implemented separately.
