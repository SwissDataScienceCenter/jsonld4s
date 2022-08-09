/*
 * Copyright 2022 Swiss Data Science Center (SDSC)
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

package io.renku.jsonld.ontology

import DataPropertyRange._
import io.renku.jsonld.JsonLDEncoder._
import io.renku.jsonld._
import io.renku.jsonld.generators.Generators.Implicits._
import io.renku.jsonld.generators.JsonLDGenerators._
import io.renku.jsonld.syntax._
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

class OntologySpec extends AnyWordSpec with should.Matchers {

  import LeafType._

  "generateOntology" should {

    "allow generating ontology for a type without nested types" in {

      generateOntology(leafType, ontologyId) shouldBe JsonLD.arr(
        JsonLD
          .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::

          JsonLD
            .entity(leafType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain"        -> List(leafType.clazz.id).asJsonLD,
              rdfs / "range"         -> List(DataPropertyRange(xsd / "string")).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::
          JsonLD
            .entity(
              schema / "number",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain"        -> List(leafType.clazz.id).asJsonLD,
              rdfs / "range"         -> List(DataPropertyRange(xsd / "number")).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::
          Nil: _*
      )
    }

    "allow generating ontology for a type with nested types" in {
      val rootType = Type.Def(
        Class(schema / "Root"),
        ObjectProperties(ObjectProperty(schema / "leaf", leafType)),
        DataProperties(DataProperty(schema / "name", xsd / "string"))
      )

      generateOntology(rootType, ontologyId) shouldBe JsonLD.arr(
        JsonLD
          .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::

          JsonLD
            .entity(rootType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::
          JsonLD
            .entity(leafType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "leaf",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain"        -> List(rootType.clazz.id).asJsonLD,
              rdfs / "range"         -> List(ObjectPropertyRange(leafType)).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::

          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain"        -> List(rootType.clazz.id, leafType.clazz.id).asJsonLD,
              rdfs / "range"         -> List(DataPropertyRange(xsd / "string")).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::
          JsonLD
            .entity(
              schema / "number",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain"        -> List(leafType.clazz.id).asJsonLD,
              rdfs / "range"         -> List(DataPropertyRange(xsd / "number")).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::
          Nil: _*
      )
    }

    "allow generating ontology when on object property can be of multiple types" in {

      val otherLeafType = Type.Def(
        Class(schema / "OtherLeaf"),
        DataProperty(schema / "name", xsd / "string")
      )

      val rootType = Type.Def(
        Class(schema / "Root"),
        ObjectProperties(ObjectProperty(schema / "leaf", leafType), ObjectProperty(schema / "leaf", otherLeafType)),
        DataProperties(DataProperty(schema / "name", xsd / "string"))
      )

      generateOntology(rootType, ontologyId) shouldBe JsonLD.arr(
        JsonLD
          .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::

          JsonLD
            .entity(rootType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::
          JsonLD
            .entity(otherLeafType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::
          JsonLD
            .entity(leafType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "leaf",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain" -> List(rootType.clazz.id).asJsonLD,
              rdfs / "range"  -> List(ObjectPropertyRange(leafType), ObjectPropertyRange(otherLeafType)).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::

          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain"        -> List(rootType.clazz.id, leafType.clazz.id, otherLeafType.clazz.id).asJsonLD,
              rdfs / "range"         -> List(DataPropertyRange(xsd / "string")).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::
          JsonLD
            .entity(
              schema / "number",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain"        -> List(leafType.clazz.id).asJsonLD,
              rdfs / "range"         -> List(DataPropertyRange(xsd / "number")).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::
          Nil: _*
      )
    }

    "allow generating ontology when one object property of different types is defined on multiple types" in {

      val yetAnotherLeafType = Type.Def(
        Class(schema / "YetAnotherLeaf"),
        DataProperty(schema / "name", xsd / "string")
      )

      val otherLeafType = Type.Def(
        Class(schema / "OtherLeaf"),
        ObjectProperty(schema / "leaf", yetAnotherLeafType)
      )

      val rootType = Type.Def(
        Class(schema / "Root"),
        ObjectProperties(ObjectProperty(schema / "leaf", leafType),
                         ObjectProperty(schema / "otherLeaf", otherLeafType)
        ),
        DataProperties(DataProperty(schema / "name", xsd / "string"))
      )

      generateOntology(rootType, ontologyId) shouldBe JsonLD.arr(
        JsonLD
          .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::

          JsonLD
            .entity(rootType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::
          JsonLD
            .entity(leafType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::
          JsonLD
            .entity(otherLeafType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::
          JsonLD
            .entity(yetAnotherLeafType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "leaf",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain" -> List(rootType.clazz.id, otherLeafType.clazz.id).asJsonLD,
              rdfs / "range"  -> List(ObjectPropertyRange(leafType), ObjectPropertyRange(yetAnotherLeafType)).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::
          JsonLD
            .entity(
              schema / "otherLeaf",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain"        -> List(rootType.clazz.id).asJsonLD,
              rdfs / "range"         -> List(ObjectPropertyRange(otherLeafType)).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::

          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(rootType.clazz.id, leafType.clazz.id, yetAnotherLeafType.clazz.id).asJsonLD,
              rdfs / "range"  -> List(DataPropertyRange(xsd / "string")).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::
          JsonLD
            .entity(
              schema / "number",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain"        -> List(leafType.clazz.id).asJsonLD,
              rdfs / "range"         -> List(DataPropertyRange(xsd / "number")).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::
          Nil: _*
      )
    }

    "allow generating ontology when one data property of different types is defined on multiple types" in {

      val rootType = Type.Def(
        Class(schema / "Root"),
        ObjectProperties(ObjectProperty(schema / "leaf", leafType)),
        DataProperties(DataProperty(schema / "name", xsd / "string"),
                       DataProperty(schema / "number", xsd / "long"),
                       DataProperty(schema / "number", xsd / "byte")
        )
      )

      generateOntology(rootType, ontologyId) shouldBe JsonLD.arr(
        JsonLD
          .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::

          JsonLD
            .entity(rootType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::
          JsonLD
            .entity(leafType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "leaf",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain"        -> List(rootType.clazz.id).asJsonLD,
              rdfs / "range"         -> List(ObjectPropertyRange(leafType)).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::

          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain"        -> List(rootType.clazz.id, leafType.clazz.id).asJsonLD,
              rdfs / "range"         -> List(DataPropertyRange(xsd / "string")).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::
          JsonLD
            .entity(
              schema / "number",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(rootType.clazz.id, leafType.clazz.id).asJsonLD,
              rdfs / "range" -> List(DataPropertyRange(xsd / "byte"),
                                     DataPropertyRange(xsd / "number"),
                                     DataPropertyRange(xsd / "long")
              ).asJsonLD,
              rdfs / "subPropertyOf" -> JsonLD.Null
            ) ::
          Nil: _*
      )
    }

    "add rdfs:subPropertyOf if defined" in {

      val otherLeafType = Type.Def(
        Class(schema / "OtherLeaf"),
        DataProperty.top(schema / "name", xsd / "string")
      )

      val rootType = Type.Def(
        Class(schema / "Root"),
        ObjectProperty.top(schema / "leaf", otherLeafType)
      )

      generateOntology(rootType, ontologyId) shouldBe JsonLD.arr(
        JsonLD
          .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::

          JsonLD
            .entity(rootType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::
          JsonLD
            .entity(otherLeafType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "leaf",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain"        -> List(rootType.clazz.id).asJsonLD,
              rdfs / "range"         -> List(ObjectPropertyRange(otherLeafType)).asJsonLD,
              rdfs / "subPropertyOf" -> EntityId.of(owl / "topObjectProperty").asJsonLD
            ) ::

          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain"        -> List(otherLeafType.clazz.id).asJsonLD,
              rdfs / "range"         -> List(DataPropertyRange(xsd / "string")).asJsonLD,
              rdfs / "subPropertyOf" -> EntityId.of(owl / "topDataProperty").asJsonLD
            ) ::
          Nil: _*
      )
    }
  }

  private lazy val ontologyId = entityIds.generateOne
  private lazy val schema     = schemas.generateOne

  private object LeafType {
    val leafType: Type = Type.Def(
      Class(schema / "Leaf"),
      DataProperty(schema / "name", xsd / "string"),
      DataProperty(schema / "number", xsd / "number")
    )
  }
}
