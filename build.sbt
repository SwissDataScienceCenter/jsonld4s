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

lazy val root = Project(
  id = "jsonld4s",
  base = file(".")
)
  .configs(Examples)
  .settings(
    organization := "io.renku",
    name := "jsonld4s",
    scalaVersion := "2.13.9",
    inConfig(Examples)(Defaults.testSettings)
  )
  .enablePlugins(AutomateHeaderPlugin)

lazy val Examples = config("examples") extend Test
headerSettings(Examples)
automateHeaderSettings(Examples)

val circeVersion = "0.14.3"
libraryDependencies += "io.circe" %% "circe-core"    % circeVersion
libraryDependencies += "io.circe" %% "circe-literal" % circeVersion
libraryDependencies += "io.circe" %% "circe-parser"  % circeVersion

libraryDependencies += "org.apache.jena" % "jena-core" % "4.6.1"
libraryDependencies += "org.typelevel"  %% "cats-core" % "2.8.0"

// Test dependencies
libraryDependencies += "eu.timepit"        %% "refined"         % "0.10.1"  % Test
libraryDependencies += "org.scalacheck"    %% "scalacheck"      % "1.17.0"  % Test // version 1.15.1 is broken
libraryDependencies += "org.scalamock"     %% "scalamock"       % "5.2.0"   % Test
libraryDependencies += "org.scalatest"     %% "scalatest"       % "3.2.14"  % Test
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test

//Compile / sourceGenerators +=
//  generate((Compile / sourceManaged).value / "examples")

ThisBuild / organizationName := "SwissDataScienceCenter"
ThisBuild / organizationHomepage := Some(url("https://www.datascience.ch"))

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

ThisBuild / description := "Scala Circe extension for JSON-LD"
ThisBuild / homepage := Some(url("https://github.com/SwissDataScienceCenter/jsonld4s"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / versionScheme := Some("early-semver")

// Sonatype plugin stuff
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"

publishTo := sonatypePublishToBundle.value

import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  setReleaseVersion,
  commitReleaseVersion,
  releaseStepCommandAndRemaining("+publishSigned"),
  releaseStepCommand("sonatypeBundleRelease"),
  setNextVersion,
  commitNextVersion,
  pushChanges
)

releaseUseGlobalVersion := false
releaseVersionBump := sbtrelease.Version.Bump.Minor

organizationName := "Swiss Data Science Center (SDSC)"
startYear := Some(java.time.LocalDate.now().getYear)
licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))
headerLicense := Some(
  HeaderLicense.Custom(
    s"""|Copyright ${java.time.LocalDate.now().getYear} Swiss Data Science Center (SDSC)
        |A partnership between École Polytechnique Fédérale de Lausanne (EPFL) and
        |Eidgenössische Technische Hochschule Zürich (ETHZ).
        |
        |Licensed under the Apache License, Version 2.0 (the "License");
        |you may not use this file except in compliance with the License.
        |You may obtain a copy of the License at
        |
        |    http://www.apache.org/licenses/LICENSE-2.0
        |
        |Unless required by applicable law or agreed to in writing, software
        |distributed under the License is distributed on an "AS IS" BASIS,
        |WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
        |See the License for the specific language governing permissions and
        |limitations under the License.""".stripMargin
  )
)
