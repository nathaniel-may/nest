name := "nest"
version := "0.1"
scalaVersion := "2.12.8"

lazy val nest = (project in file("."))
  .settings(
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.26",

    libraryDependencies += "org.scalatest"  %% "scalatest"   % "3.0.5"  % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck"  % "1.14.0" % "test"
  )