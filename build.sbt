name := "units-of-measure"

version := "0.1"

// LMS requires scala 2.11(.2?), not 2.12
scalaVersion := "2.11.2"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.5" % "test"

initialCommands in console := "import uk.ac.cam.jaw89.metaprogramming.units_of_measure._; import uk.ac.cam.jaw89.metaprogramming.units_of_measure.SI._"

// LMS
scalaOrganization := "org.scala-lang.virtualized"

resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "org.scala-lang.lms" %% "lms-core" % "1.0.0-SNAPSHOT"
libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.11.2"
libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % "2.11.2"
libraryDependencies += "org.scala-lang.virtualized" % "scala-reflect" % "2.11.2"

scalacOptions += "-Yvirtualize"
scalacOptions += "-feature"
