name := "example"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.8.1",
  "org.scalaz.stream" %% "scalaz-stream" % "0.8.5",
  "com.typesafe.akka" %% "akka-actor" % "2.4.16",
  "com.typesafe.akka" %% "akka-stream" % "2.4.16"
)
