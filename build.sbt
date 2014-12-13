scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  // main
  "org.scalaz"     %% "scalaz-core" % "7.1.0",
  // test
  "org.scalatest"  %% "scalatest"   % "2.2.1"  % "test",
  "org.scalacheck" %% "scalacheck"  % "1.12.1" % "test"
)
