name := "checkout"

version := "0.1"

scalaVersion := "2.12.8"

resolvers += "Mavenrepository" at "https://repo1.maven.org/maven2/"

val scalaTestVersion = "3.0.1"

libraryDependencies ++= Seq(
  "org.scalatest"           %% "scalatest"                  % scalaTestVersion  % "test"
)
