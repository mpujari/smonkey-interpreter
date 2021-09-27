ThisBuild / version := "0.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

trapExit := false

scalacOptions ++= Seq("-unchecked", "-deprecation")
