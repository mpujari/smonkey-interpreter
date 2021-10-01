ThisBuild / version := "1.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

trapExit := false

scalacOptions ++= Seq("-unchecked", "-deprecation")
