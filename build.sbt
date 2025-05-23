val scala3Version = "3.7.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fsm_as_macro",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )

libraryDependencies ++= Seq(
  "org.scala-lang" %% "scala3-library" % scalaVersion.value
)
