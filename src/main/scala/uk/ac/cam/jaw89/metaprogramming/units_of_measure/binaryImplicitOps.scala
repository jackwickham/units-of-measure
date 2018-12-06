package uk.ac.cam.jaw89.metaprogramming.units_of_measure

trait BinaryNumeric[A, B] {
  def plus(a: A, b: B): A
  def minus(a: A, b: B): A
  def times(a: A, b: B): A
}

trait BinaryFractional[A, B] extends BinaryNumeric[A, B] {
  def div(a: A, b: B): A
}

object BinaryImplicitOps {
  import scala.language.implicitConversions

  /**
    * Provide an instance of BinaryNumeric[A, A] when a Numeric[A] is available
    */
  implicit def genBinaryNumeric[A](implicit num: Numeric[A]): BinaryNumeric[A, A] = new BinaryNumeric[A, A] {
    def plus(a: A, b: A): A = num.plus(a, b)
    def minus(a: A, b: A): A = num.minus(a, b)
    def times(a: A, b: A): A = num.times(a, b)
  }

  /**
    * Provide an instance of BinaryNumeric[A, A] when a Numeric[A] is available
    */
  implicit def genBinaryFractional[A](implicit num: Fractional[A]): BinaryFractional[A, A] = new BinaryFractional[A, A] {
    def plus(a: A, b: A): A = num.plus(a, b)
    def minus(a: A, b: A): A = num.minus(a, b)
    def times(a: A, b: A): A = num.times(a, b)
    def div(a: A, b: A): A = num.div(a, b)
  }
}
