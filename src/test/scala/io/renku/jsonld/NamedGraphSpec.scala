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
import cats.syntax.all._
import io.circe.Json
import io.circe.literal._
import io.renku.jsonld.JsonLD.JsonLDEntity
import io.renku.jsonld.generators.Generators.Implicits._
import io.renku.jsonld.generators.JsonLDGenerators._
import org.scalacheck.Gen
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import syntax._

class NamedGraphSpec extends AnyWordSpec with should.Matchers with ScalaCheckPropertyChecks {

  "apply" should {

    "construct a NamedGraph with the given EntityId and JsonLDEntity" in {
      val id     = entityIds.generateOne
      val entity = jsonLDEntities.generateOne

      val graph = NamedGraph(id, entity)

      graph.id       shouldBe id
      graph.entities shouldBe List(entity)
    }

    "construct a NamedGraph with the given EntityId and JsonLDEntities" in {
      val id            = entityIds.generateOne
      val entity        = jsonLDEntities.generateOne
      val otherEntities = jsonLDEntities.generateNonEmptyList().toList

      val graph = NamedGraph(id, entity, otherEntities: _*)

      graph.id       shouldBe id
      graph.entities shouldBe entity +: otherEntities
    }
  }

  "from" should {

    "allow instantiating from a JsonLDArray of JsonLDEntityLike objects" in {
      val id             = entityIds.generateOne
      val array1Entities = jsonLDEntityLikes.generateNonEmptyList().toList
      val array2Entities = jsonLDEntityLikes.generateNonEmptyList().toList

      val Right(graph) = NamedGraph.from(id, JsonLD.arr(array1Entities: _*), JsonLD.arr(array2Entities: _*))

      graph.id       shouldBe id
      graph.entities shouldBe array1Entities ::: array2Entities
    }

    "allow instantiating from a JsonLDEntityLike objects" in {
      val id      = entityIds.generateOne
      val entity1 = jsonLDEntityLikes.generateOne
      val entity2 = jsonLDEntityLikes.generateOne

      val Right(graph) = NamedGraph.from(id, entity1, entity2)

      graph.id       shouldBe id
      graph.entities shouldBe entity1 :: entity2 :: Nil
    }

    "allow instantiating from a nested JsonLDArrays of JsonLDEntityLike objects" in {
      val id       = entityIds.generateOne
      val entities = jsonLDEntityLikes.generateNonEmptyList().toList

      val Right(graph) = NamedGraph.from(id, JsonLD.arr(JsonLD.arr(entities: _*)))

      graph.id       shouldBe id
      graph.entities shouldBe entities
    }

    "return a failure if there are objects other than JsonLDEntityLike" in {
      val id       = entityIds.generateOne
      val entities = jsonLDEntityLikes.generateNonEmptyList().toList
      val other    = jsonLDValues.generateNonEmptyList().toList

      val Left(error) = NamedGraph.from(id, JsonLD.arr(entities ::: other: _*))

      error.getMessage shouldBe "NamedGraph can be instantiated with a Entities and Edges only"
    }
  }

  "fromJsonLDsUnsafe" should {

    "instantiate successfully from JsonLDEntityLike objects" in {
      val id = entityIds.generateOne
      val entities: NonEmptyList[JsonLD] = jsonLDEntityLikes.generateNonEmptyList()

      val graph = NamedGraph.fromJsonLDsUnsafe(id, entities.head, entities.tail: _*)

      graph.id       shouldBe id
      graph.entities shouldBe entities.toList
    }

    "throw a failure if there are objects other than JsonLDEntityLike" in {
      val id = entityIds.generateOne
      val entities: NonEmptyList[JsonLD] = jsonLDEntityLikes.generateNonEmptyList() :+ jsonLDValues.generateOne

      intercept[IllegalArgumentException] {
        NamedGraph.fromJsonLDsUnsafe(id, entities.head, entities.tail: _*)
      }.getMessage shouldBe "NamedGraph can be instantiated with a Entities and Edges only"
    }
  }

  "toJson" should {

    "turn the given NamedGraph into JSON" in {
      forAll { (graph: NamedGraph) =>
        graph.toJson shouldBe Json.obj(
          "@id"    -> graph.id.asJson,
          "@graph" -> Json.fromValues(graph.entities.map(_.toJson))
        )
      }
    }
  }

  "entityId" should {

    "return the NamedGraph id" in {
      val id       = entityIds.generateOne
      val entities = jsonLDEntities.generateNonEmptyList().toList

      NamedGraph(id, entities).entityId shouldBe Some(id)
    }
  }

  "entityTypes" should {

    "return None" in {
      namedGraphs.generateOne.entityTypes shouldBe None
    }
  }

  "asArray" should {

    "return a single element Array with itself" in {
      val graph = namedGraphs.generateOne

      graph.asArray shouldBe Some(Vector(graph))
    }
  }

  "merge" should {

    "return the same NamedGraph if it contains only one entity" in {
      val id     = entityIds.generateOne
      val entity = jsonLDEntities.generateOne

      val graph = NamedGraph(id, entity)

      graph.merge shouldBe graph.asRight
    }

    "return NamedGraph with merged entities in the sense of an JsonLDArray merge " +
      "if there's more than one entity" in {
        val entity1 = JsonLD.entity(
          entityIds.generateOne,
          entityTypesObject.generateOne,
          valuesProperties.generateOne
        )
        val entity2 = JsonLD.entity(
          entityIds.generateOne,
          entityTypesObject.generateOne,
          valuesProperties.generateOne
        )
        val edge = JsonLD.edge(entity1.id, properties.generateOne, entity2.id)

        val Right(mergedNamedGraph) = NamedGraph(entityIds.generateOne, entity1, entity2, edge).merge

        val Right(merged) = JsonLD.arr(entity1, entity2, edge).merge
        mergedNamedGraph.entities shouldBe merged.asArray.sequence.flatten
      }
  }

  "flatten" should {

    lazy val entityProperties: Gen[(Property, JsonLDEntity)] = for {
      property <- properties
      entity   <- jsonLDEntities
    } yield property -> entity

    implicit class JsonLDEntityOps(entity: JsonLDEntity) {

      def add(properties: List[(Property, JsonLD)]): JsonLDEntity =
        properties.foldLeft(entity) { case (entity, (property, entityValue)) =>
          entity.add(property -> entityValue)
        }

      def add(property: (Property, JsonLD)): JsonLDEntity = entity.copy(properties = entity.properties + property)
    }

    "return the same NamedGraph if it contains only one not nested entity" in {
      val id     = entityIds.generateOne
      val entity = jsonLDEntities.generateOne

      val graph = NamedGraph(id, entity)

      graph.flatten shouldBe graph.asRight
    }

    "return NamedGraph with flattened entities in the sense of an JsonLDArray merge " +
      "if there's more than one entity" in {
        forAll {
          (childlessGrandparent: JsonLDEntity, parentRelationProperty: Property, parentNotNested: JsonLDEntity) =>
            val childrenTuples       = entityProperties.generateNonEmptyList().toList
            val parentWithProperties = parentNotNested.add(childrenTuples)
            val grandparentWithChild = childlessGrandparent.add(parentRelationProperty -> parentWithProperties)

            val flattenedGrandparent =
              childlessGrandparent.add(parentRelationProperty -> JsonLD.fromEntityId(parentWithProperties.id))
            val flattenedParent = childrenTuples
              .foldLeft(parentNotNested) { case (parent, (property, child)) =>
                parent.add(property -> child.id.asJsonLD)
              }
            val children = childrenTuples.map { case (_, entity) => entity: JsonLD }

            val otherEntity = jsonLDEntities.generateOne
            val edge        = jsonLDEdges.generateOne

            val Right(flattened) = NamedGraph(entityIds.generateOne, grandparentWithChild, otherEntity, edge).flatten

            flattened.entities should contain theSameElementsAs
              flattenedGrandparent :: flattenedParent :: children ::: otherEntity :: edge :: Nil
        }
      }
  }
}
