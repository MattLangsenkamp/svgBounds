import sbt.Keys.libraryDependencies

import scala.collection.Seq

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "svgBounds",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.8",
      "org.scalameta" %% "munit" % "1.0.0-M6",
      "org.locationtech.jts" % "jts" % "1.19.0",
      "org.locationtech.jts" % "jts-core" % "1.19.0",
      "org.locationtech.jts" % "jts-io" % "1.19.0" pomOnly(),
      "org.locationtech.jts" % "jts-modules" % "1.19.0" pomOnly(),
      "org.locationtech.jts.io" % "jts-io-common" % "1.19.0",
      "org.jsoup" % "jsoup" % "1.15.3"
    )
  )
