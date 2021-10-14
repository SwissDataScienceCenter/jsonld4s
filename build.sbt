/*
 * Copyright 2019 Swiss Data Science Center (SDSC)
 * A partnership between École Polytechnique Fédérale de Lausanne (EPFL) and
 * Eidgenössische Technische Hochschule Zürich (ETHZ).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

organization := "io.renku"
name := "jsonld4s"
scalaVersion := "2.13.6"

val circeVersion = "0.14.1"
libraryDependencies += "io.circe" %% "circe-core"    % circeVersion
libraryDependencies += "io.circe" %% "circe-literal" % circeVersion
libraryDependencies += "io.circe" %% "circe-parser"  % circeVersion

libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.0"

// Test dependencies
libraryDependencies += "eu.timepit"        %% "refined"         % "0.9.26"  % Test
libraryDependencies += "org.scalacheck"    %% "scalacheck"      % "1.14.3"  % Test // version 1.15.1 is broken
libraryDependencies += "org.scalamock"     %% "scalamock"       % "5.1.0"   % Test
libraryDependencies += "org.scalatest"     %% "scalatest"       % "3.2.6"   % Test
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test

ThisBuild / organizationName := "SwissDataScienceCenter"
ThisBuild / organizationHomepage := Some(url("https://www.datascience.ch"))
ThisBuild / version := "0.1.10"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/SwissDataScienceCenter/jsonld4s"),
    "scm:git@github.com:SwissDataScienceCenter/jsonld4s.git"
  )
)

ThisBuild / developers := List(
  Developer(
    id = "SwissDataScienceCenter",
    name = "Swiss Data Science Center",
    email = "renku@datascience.ch",
    url = url("https://www.datascience.ch")
  )
)

ThisBuild / description := "Circe extension for JSON-LD parsing in Scala"
ThisBuild / licenses := List("Apache 2.0" -> new URL("http://www.apache.org/licenses/"))
ThisBuild / homepage := Some(url("https://github.com/SwissDataScienceCenter/jsonld4s"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

ThisBuild / publishMavenStyle := true

ThisBuild / versionScheme := Some("early-semver")
