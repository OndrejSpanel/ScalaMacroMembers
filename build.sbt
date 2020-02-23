version := "0.1"

name := "MacroProject"

scalaVersion in ThisProject := "2.12.10"

lazy val members = project.settings(
  name := "members",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
)

lazy val root = project.dependsOn(members).settings(
  name := "root"
)
