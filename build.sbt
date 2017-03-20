name := """rzd-presale"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

routesImport += "utils.Binders._"

libraryDependencies ++= Seq(
    jdbc,
    cache,
    ws,
    "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test
)

libraryDependencies += "org.apache.poi" % "poi" % "3.10.1"
libraryDependencies += "org.apache.poi" % "poi-ooxml" % "3.10.1"