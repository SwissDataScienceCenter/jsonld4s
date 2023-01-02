/*
 * Copyright 2023 Swiss Data Science Center (SDSC)
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

package io.renku.jsonld

import cats.data.NonEmptyList
import io.renku.jsonld.syntax._

package object ontology {

  val oa:   Schema = Schema.from("http://www.w3.org/ns/oa", separator = "#")
  val owl:  Schema = Schema.from("http://www.w3.org/2002/07/owl", separator = "#")
  val rdf:  Schema = Schema.from("http://www.w3.org/1999/02/22-rdf-syntax-ns", separator = "#")
  val rdfs: Schema = Schema.from("http://www.w3.org/2000/01/rdf-schema", separator = "#")
  val xsd:  Schema = Schema.from("http://www.w3.org/2001/XMLSchema", separator = "#")

  def generateOntology(typeDef: Type, schema: Schema): JsonLD = generateOntology(typeDef, EntityId.of(schema))

  def generateOntology(typeDef: Type, ontologyId: EntityId): JsonLD =
    generateOntology(NonEmptyList.one(typeDef), ontologyId)

  def generateOntology(typeDefs: NonEmptyList[Type], ontologyId: EntityId): JsonLD = JsonLD.arr(
    JsonLD.entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::
      typeDefs.toList.flatMap(_.asJsonLD.asArray.getOrElse(Vector.empty)): _*
  )
}
