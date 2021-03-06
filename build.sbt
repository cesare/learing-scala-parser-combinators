lazy val root = (project in file(".")).
  settings(
    name := "learing-parser-combinators",
    version := "0.1.0",
    scalaVersion := "2.11.4"
  )

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
)
