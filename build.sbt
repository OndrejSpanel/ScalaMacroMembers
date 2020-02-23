name := "Macros"

version := "0.1"

scalaVersion in ThisProject := "2.12.10"

lazy val macros = project.settings(
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
)

lazy val root = project.dependsOn(macros)
