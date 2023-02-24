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
    crossScalaVersions := Seq("2.13.10", "2.12.12"),
    scalaVersion := "2.13.10",
    inConfig(Examples)(Defaults.testSettings)
  )
  .enablePlugins(AutomateHeaderPlugin)

lazy val Examples = config("examples") extend Test
headerSettings(Examples)
automateHeaderSettings(Examples)

val circeVersion = "0.14.4"
libraryDependencies += "io.circe" %% "circe-core"    % circeVersion
libraryDependencies += "io.circe" %% "circe-literal" % circeVersion
libraryDependencies += "io.circe" %% "circe-parser"  % circeVersion

libraryDependencies += "org.apache.jena" % "jena-core" % "4.7.0"
libraryDependencies += "org.typelevel"  %% "cats-core" % "2.9.0"

// Test dependencies
libraryDependencies += "eu.timepit"        %% "refined"         % "0.10.1"  % Test
libraryDependencies += "org.scalacheck"    %% "scalacheck"      % "1.17.0"  % Test
libraryDependencies += "org.scalamock"     %% "scalamock"       % "5.2.0"   % Test
libraryDependencies += "org.scalatest"     %% "scalatest"       % "3.2.15"  % Test
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test

inThisBuild(
  List(
    homepage := Some(url("https://www.datascience.ch")),
    licenses := List("Apache 2.0" -> new URL("http://www.apache.org/licenses/")),
    description := "Scala Circe extension for JSON-LD",
    homepage := Some(url("https://github.com/SwissDataScienceCenter/jsonld4s")),
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full),
    developers := List(
      Developer(
        id = "SwissDataScienceCenter",
        name = "Swiss Data Science Center",
        email = "renku@datascience.ch",
        url = url("https://www.datascience.ch")
      )
    ),
    versionScheme := Some("early-semver"),
    sonatypeProfileName := "io.renku",
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
    dynverVTagPrefix := false
  )
)

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

addCommandAlias("testAll", "+ test")

