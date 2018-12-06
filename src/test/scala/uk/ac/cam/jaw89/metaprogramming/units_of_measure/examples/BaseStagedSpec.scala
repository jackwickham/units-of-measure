package uk.ac.cam.jaw89.metaprogramming.units_of_measure.examples

import uk.ac.cam.jaw89.metaprogramming.units_of_measure.TestSpec

import scala.io.Source

abstract class BaseStagedSpec extends TestSpec {
  def readExpectedResult(name: String): String = {
    val source = Source.fromFile("src/test/resources/expected_results/" + name + ".out")
    val result = source.mkString
    source.close()
    result.trim
  }
}
