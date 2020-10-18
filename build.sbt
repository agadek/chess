name := "chess"

version := "0.1"

scalaVersion := "2.13.3"

val Versions = new {
  val scalatest = "3.2.0"
}

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-encoding",
  "utf8",
  "-feature",
  "-Ybreak-cycles",
  "-Xfatal-warnings")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % Versions.scalatest % "test"
)