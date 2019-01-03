package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure._
import uk.ac.cam.jaw89.metaprogramming.units_of_measure.SI._

/**
  * An example showing an alternative datatype for the measurements - a vector3 - and a polymortphic function that acts
  * on the values regardless of their type
  */
object Vectors {
  case class Vec3[T](x: T, y: T, z: T) {
    def componentUnaryOp[A](op: T => A): Vec3[A] = Vec3[A](op(x), op(y), op(z))
    def componentBinaryOp[A, U](other: Vec3[U], op: (T, U) => A): Vec3[A] = Vec3[A](op(x, other.x), op(y, other.y), op(z, other.z))
  }
  
  implicit object Vec3DblFractional extends Fractional[Vec3[Double]] with MultiplyAndExponentiate[Vec3[Double]] {
    override def fromInt(x: Int): Vec3[Double] = Vec3(x.toDouble, x.toDouble, x.toDouble)
    override def compare(x: Vec3[Double], y: Vec3[Double]): Int = throw new UnsupportedOperationException()
    override def div(x: Vec3[Double], y: Vec3[Double]): Vec3[Double] = x.componentBinaryOp(y, _ / (_: Double))
    override def minus(x: Vec3[Double], y: Vec3[Double]): Vec3[Double] = x.componentBinaryOp(y, _ - (_: Double))
    override def plus(x: Vec3[Double], y: Vec3[Double]): Vec3[Double] = x.componentBinaryOp(y, _ + (_: Double))
    override def times(x: Vec3[Double], y: Vec3[Double]): Vec3[Double] = x.componentBinaryOp(y, _ * (_: Double))
    override def negate(x: Vec3[Double]): Vec3[Double] = x.componentUnaryOp(- _)
    override def toDouble(x: Vec3[Double]): Double = throw new UnsupportedOperationException()
    override def toFloat(x: Vec3[Double]): Float = throw new UnsupportedOperationException()
    override def toInt(x: Vec3[Double]): Int = throw new UnsupportedOperationException()
    override def toLong(x: Vec3[Double]): Long = throw new UnsupportedOperationException()

    override def times(x: Vec3[Double], y: Multiplier): Vec3[Double] = x.componentUnaryOp(_ * y)
    override def div(x: Vec3[Double], y: Multiplier): Vec3[Double] = x.componentUnaryOp(_ / y)
  }

  implicit object Vec3DblAndDblBinaryFractional extends BinaryFractional[Vec3[Double], Double] {
    override def div(x: Vec3[Double], y: Double): Vec3[Double] = x.componentUnaryOp(_ / y)
    override def minus(x: Vec3[Double], y: Double): Vec3[Double] = x.componentUnaryOp(_ - y)
    override def plus(x: Vec3[Double], y: Double): Vec3[Double] = x.componentUnaryOp(_ + y)
    override def times(x: Vec3[Double], y: Double): Vec3[Double] = x.componentUnaryOp(_ * y)
  }

  /**
    * A unit-invariant numerical differentiation function - given a list of points, it calculates the rate of change of
    * the value at each time step
    */
  def differentiate(points: List[Measurement[Vec3[Double]]], interval: Measurement[Double], incomingUnit: Option[DerivedUnit] = None): List[Measurement[Vec3[Double]]] = {
    points match {
      case first :: second :: tl =>
        val unit = incomingUnit.getOrElse(first.unit)
        ((second.in(unit) - first.in(unit)) / interval) :: differentiate(second::tl, interval, Some(unit))
      case _ => Nil
    }
  }

  def main(args: Array[String]): Unit = {
    val positions: List[Measurement[Vec3[Double]]] = List(
      Vec3(100.0, 5.0, 60.0)(m),
      Vec3(120.8, 5.5, 50.0)(m),
      Vec3(80.5, 4.8, 49.2)(m),
      Vec3(21.0, 3.1, 41.7)(m)
    )
    val interval = 5.0(s)
    val velocities = differentiate(positions, interval)
    val accelerations = differentiate(velocities, interval)
    val jerk = differentiate(accelerations, interval).head
    println(positions)
    println(velocities)
    println(accelerations)
    println(jerk)
  }
}
