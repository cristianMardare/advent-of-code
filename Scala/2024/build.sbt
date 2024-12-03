val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "2024",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.7",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,

    scalacOptions += "-Wnonunit-statement"
  )
