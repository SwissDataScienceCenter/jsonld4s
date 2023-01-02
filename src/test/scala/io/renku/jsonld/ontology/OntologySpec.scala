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

package io.renku.jsonld.ontology

import DataPropertyRange._
import cats.data.NonEmptyList
import cats.syntax.all._
import io.renku.jsonld.JsonLDDecoder._
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

      generateOntology(leaf, ontologyId) shouldBe JsonLD.arr(
        JsonLD
          .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::

          JsonLD
            .entity(leaf.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(leaf.clazz.id).asJsonLD,
              rdfs / "range"  -> List(DataPropertyRange(xsd / "string")).asJsonLD
            ) ::
          JsonLD
            .entity(
              schema / "number",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(leaf.clazz.id).asJsonLD,
              rdfs / "range"  -> List(DataPropertyRange(xsd / "number")).asJsonLD
            ) ::
          Nil: _*
      )
    }

    "allow generating ontology for a type with nested types" in {
      val rootType = Type.Def(
        Class(schema / "Root"),
        ObjectProperties(ObjectProperty(schema / "leaf", leaf)),
        DataProperties(DataProperty(schema / "name", xsd / "string"))
      )

      generateOntology(rootType, ontologyId) shouldBe JsonLD.arr(
        JsonLD
          .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::

          JsonLD
            .entity(rootType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::
          JsonLD
            .entity(leaf.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "leaf",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain" -> List(rootType.clazz.id).asJsonLD,
              rdfs / "range"  -> List(ObjectPropertyRange(leaf)).asJsonLD
            ) ::

          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(rootType.clazz.id, leaf.clazz.id).asJsonLD,
              rdfs / "range"  -> List(DataPropertyRange(xsd / "string")).asJsonLD
            ) ::
          JsonLD
            .entity(
              schema / "number",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(leaf.clazz.id).asJsonLD,
              rdfs / "range"  -> List(DataPropertyRange(xsd / "number")).asJsonLD
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
        ObjectProperties(ObjectProperty(schema / "leaf", leaf), ObjectProperty(schema / "leaf", otherLeafType)),
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
            .entity(leaf.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "leaf",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain" -> List(rootType.clazz.id).asJsonLD,
              rdfs / "range"  -> List(ObjectPropertyRange(leaf), ObjectPropertyRange(otherLeafType)).asJsonLD
            ) ::

          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(rootType.clazz.id, leaf.clazz.id, otherLeafType.clazz.id).asJsonLD,
              rdfs / "range"  -> List(DataPropertyRange(xsd / "string")).asJsonLD
            ) ::
          JsonLD
            .entity(
              schema / "number",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(leaf.clazz.id).asJsonLD,
              rdfs / "range"  -> List(DataPropertyRange(xsd / "number")).asJsonLD
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
        ObjectProperties(ObjectProperty(schema / "leaf", leaf), ObjectProperty(schema / "otherLeaf", otherLeafType)),
        DataProperties(DataProperty(schema / "name", xsd / "string"))
      )

      generateOntology(rootType, ontologyId) shouldBe JsonLD.arr(
        JsonLD
          .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::

          JsonLD
            .entity(rootType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::
          JsonLD
            .entity(leaf.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::
          JsonLD
            .entity(otherLeafType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::
          JsonLD
            .entity(yetAnotherLeafType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "leaf",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain" -> List(rootType.clazz.id, otherLeafType.clazz.id).asJsonLD,
              rdfs / "range"  -> List(ObjectPropertyRange(leaf), ObjectPropertyRange(yetAnotherLeafType)).asJsonLD
            ) ::
          JsonLD
            .entity(
              schema / "otherLeaf",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain" -> List(rootType.clazz.id).asJsonLD,
              rdfs / "range"  -> List(ObjectPropertyRange(otherLeafType)).asJsonLD
            ) ::

          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(rootType.clazz.id, leaf.clazz.id, yetAnotherLeafType.clazz.id).asJsonLD,
              rdfs / "range"  -> List(DataPropertyRange(xsd / "string")).asJsonLD
            ) ::
          JsonLD
            .entity(
              schema / "number",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(leaf.clazz.id).asJsonLD,
              rdfs / "range"  -> List(DataPropertyRange(xsd / "number")).asJsonLD
            ) ::
          Nil: _*
      )
    }

    "allow generating ontology when one data property of different types is defined on multiple types" in {

      val rootType = Type.Def(
        Class(schema / "Root"),
        ObjectProperties(ObjectProperty(schema / "leaf", leaf)),
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
            .entity(leaf.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "leaf",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain" -> List(rootType.clazz.id).asJsonLD,
              rdfs / "range"  -> List(ObjectPropertyRange(leaf)).asJsonLD
            ) ::

          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(rootType.clazz.id, leaf.clazz.id).asJsonLD,
              rdfs / "range"  -> List(DataPropertyRange(xsd / "string")).asJsonLD
            ) ::
          JsonLD
            .entity(
              schema / "number",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(rootType.clazz.id, leaf.clazz.id).asJsonLD,
              rdfs / "range" -> List(DataPropertyRange(xsd / "byte"),
                                     DataPropertyRange(xsd / "number"),
                                     DataPropertyRange(xsd / "long")
              ).asJsonLD
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

    "list subClasses if defined on the Type" in {

      val otherLeafType = Type.Def(
        Class(schema / "OtherLeaf", ParentClass(schema / "Top"))
      )

      val rootType = Type.Def(
        Class(schema / "Root", ParentClass(leaf)),
        ObjectProperty(schema / "leaf", otherLeafType)
      )

      generateOntology(rootType, ontologyId) shouldBe JsonLD.arr(
        JsonLD
          .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::

          JsonLD
            .entity(rootType.clazz.id,
                    EntityTypes of owl / "Class",
                    rdfs / "subClassOf" -> List(leaf.clazz.id).asJsonLD
            ) ::
          JsonLD
            .entity(otherLeafType.clazz.id,
                    EntityTypes of owl / "Class",
                    rdfs / "subClassOf" -> List(EntityId.of(schema / "Top")).asJsonLD
            ) ::

          JsonLD
            .entity(
              schema / "leaf",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain" -> List(rootType.clazz.id).asJsonLD,
              rdfs / "range"  -> List(ObjectPropertyRange(otherLeafType)).asJsonLD
            ) ::
          Nil: _*
      )
    }

    "include comments if added on Class, ObjectProperty and/or DataProperty" in {

      val otherLeafType = Type.Def(
        Class(schema / "OtherLeaf"),
        DataProperty(schema / "name", xsd / "string").withComment("Data prop comment")
      )

      val rootType = Type.Def(
        Class(schema / "Root", Some(Comment("Class comment"))),
        ObjectProperty(schema / "leaf", otherLeafType, Some(Comment("Object prop comment")))
      )

      generateOntology(rootType, ontologyId) shouldBe JsonLD.arr(
        JsonLD
          .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::

          JsonLD
            .entity(rootType.clazz.id, EntityTypes of owl / "Class", rdfs / "comment" -> "Class comment".asJsonLD) ::
          JsonLD
            .entity(otherLeafType.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "leaf",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain"  -> List(rootType.clazz.id).asJsonLD,
              rdfs / "range"   -> List(ObjectPropertyRange(otherLeafType)).asJsonLD,
              rdfs / "comment" -> "Object prop comment".asJsonLD
            ) ::

          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain"  -> List(otherLeafType.clazz.id).asJsonLD,
              rdfs / "range"   -> List(DataPropertyRange(xsd / "string")).asJsonLD,
              rdfs / "comment" -> "Data prop comment".asJsonLD
            ) ::

          Nil: _*
      )
    }

    "allow generating ontology when there's a Data Property with an enum range" in {

      val typeDef = Type.Def(
        Class(schema / "Type"),
        DataProperty(schema / "enum", DataPropertyRange("a", "b"))
      )

      generateOntology(typeDef, ontologyId).flatten
        .fold(throw _, identity)
        .cursor
        .as(decodeList(enumFieldDecoder))
        .map(_.flatten) shouldBe List("a", "b").asRight

      lazy val enumFieldDecoder = JsonLDDecoder.entity(EntityTypes of owl / "DatatypeProperty") { cursor =>
        for {
          id <- cursor.downEntityId.as[EntityId]
          _ <- Either.cond(id == EntityId.of(schema / "enum"),
                           (),
                           fail(s"DatatypeProperty id: $id but expected ${EntityId.of(schema / "enum")}")
               )
          rangeId <- cursor.downField(rdfs / "range").as[EntityId]
          oneOfId <- cursor.focusTop
                       .as(decodeList(datatypeDecoder))
                       .map(_.find(_._1 == rangeId).map(_._2).getOrElse(fail("No Datatype entity")))
          list       <- cursor.focusTop.as(decodeList(listDecoder))
          firstItem  <- list.find(_._1 == oneOfId).getOrElse(fail("No first enum item")).asRight
          secondItem <- list.find(_._1 == firstItem._3).getOrElse(fail("No second enum item")).asRight
          _          <- Either.cond(secondItem._3 == EntityId.of(rdf / "nil"), (), fail("More than two items found"))
        } yield List(firstItem._2, secondItem._2)
      }

      lazy val datatypeDecoder = JsonLDDecoder.entity(EntityTypes of rdfs / "Datatype") { cur =>
        (cur.downEntityId.as[EntityId] -> cur.downField(owl / "oneOf").as[EntityId]).mapN(_ -> _)
      }

      lazy val listDecoder = JsonLDDecoder.entity(EntityTypes of rdf / "List") { cursor =>
        for {
          id     <- cursor.downEntityId.as[EntityId]
          first  <- cursor.downField(rdf / "first").as[String]
          restId <- cursor.downField(rdf / "rest").downEntityId.as[EntityId]
        } yield (id, first, restId)
      }
    }

    "allow generating ontology with circular dependencies" in {

      lazy val root = Type.Def(
        Class(schema / "Root"),
        ObjectProperty(schema / "root", Class(schema / "Root"))
      )

      generateOntology(root, ontologyId) shouldBe JsonLD.arr(
        JsonLD
          .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::

          JsonLD
            .entity(root.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "root",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain" -> List(root.clazz.id).asJsonLD,
              rdfs / "range"  -> List(ObjectPropertyRange(root)).asJsonLD
            ) ::
          Nil: _*
      )
    }

    "allow defining reverse dependencies" in {

      val reverseClass = Class(schema / "Reverse")
      lazy val reverse = ReverseProperty(refTypeClass =>
        Type.Def(
          reverseClass,
          ObjectProperties(ObjectProperty(schema / "root", refTypeClass)),
          DataProperties(DataProperty(schema / "name", xsd / "string"))
        )
      )

      lazy val root = Type.Def(
        Class(schema / "Root"),
        ObjectProperties.empty,
        DataProperties(DataProperty(schema / "name", xsd / "string")),
        ReverseProperties(reverse)
      )

      generateOntology(root, ontologyId) shouldBe JsonLD.arr(
        JsonLD
          .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)) ::

          JsonLD
            .entity(root.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::
          JsonLD
            .entity(reverseClass.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]) ::

          JsonLD
            .entity(
              schema / "root",
              EntityTypes of owl / "ObjectProperty",
              rdfs / "domain" -> List(reverseClass.id).asJsonLD,
              rdfs / "range"  -> List(ObjectPropertyRange(root)).asJsonLD
            ) ::

          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(root.clazz.id, reverseClass.id).asJsonLD,
              rdfs / "range"  -> List(DataPropertyRange(xsd / "string")).asJsonLD
            ) ::
          Nil: _*
      )
    }

    "allow two unrelated types" in {
      val root1 = Type.Def(
        Class(schema / "Root1"),
        ObjectProperties.empty,
        DataProperties(DataProperty(schema / "name", xsd / "string"))
      )
      val root2 = Type.Def(
        Class(schema / "Root2"),
        ObjectProperties.empty,
        DataProperties(DataProperty(schema / "description", xsd / "string"))
      )

      generateOntology(NonEmptyList.of(root1, root2), ontologyId) shouldBe JsonLD.arr(
        List(
          JsonLD
            .entity(ontologyId, EntityTypes of owl / "Ontology", owl / "imports" -> JsonLD.arr(oa.asJsonLD)),
          JsonLD
            .entity(root1.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]),
          JsonLD
            .entity(
              schema / "name",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(root1.clazz.id).asJsonLD,
              rdfs / "range"  -> List(DataPropertyRange(xsd / "string")).asJsonLD
            ),
          JsonLD
            .entity(root2.clazz.id, EntityTypes of owl / "Class", Map.empty[Property, JsonLD]),
          JsonLD
            .entity(
              schema / "description",
              EntityTypes of owl / "DatatypeProperty",
              rdfs / "domain" -> List(root2.clazz.id).asJsonLD,
              rdfs / "range"  -> List(DataPropertyRange(xsd / "string")).asJsonLD
            )
        ): _*
      )
    }
  }

  private lazy val ontologyId = entityIds.generateOne
  private lazy val schema     = schemas.generateOne

  private object LeafType {
    val leaf: Type = Type.Def(
      Class(schema / "Leaf"),
      DataProperty(schema / "name", xsd / "string"),
      DataProperty(schema / "number", xsd / "number")
    )
  }
}
