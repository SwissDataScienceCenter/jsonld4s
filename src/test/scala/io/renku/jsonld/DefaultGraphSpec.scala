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
import io.circe.literal._
import io.renku.jsonld.JsonLD.JsonLDEntity
import io.renku.jsonld.generators.Generators.Implicits._
import io.renku.jsonld.generators.JsonLDGenerators._
import org.scalacheck.Gen
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import syntax._

class DefaultGraphSpec extends AnyWordSpec with should.Matchers with ScalaCheckPropertyChecks {

  "apply" should {

    "construct a NamedGraph with the given JsonLDEntity" in {
      val entity = jsonLDEntities.generateOne

      val graph = DefaultGraph(entity)

      graph.entities shouldBe List(entity)
    }

    "construct a NamedGraph with the given JsonLDEntities" in {
      val entity        = jsonLDEntities.generateOne
      val otherEntities = jsonLDEntities.generateNonEmptyList().toList

      val graph = DefaultGraph(entity, otherEntities: _*)

      graph.entities shouldBe entity +: otherEntities
    }
  }

  "from" should {

    "allow instantiating from a JsonLDArray of JsonLDEntityLike objects" in {
      val array1Entities = jsonLDEntityLikes.generateNonEmptyList().toList
      val array2Entities = jsonLDEntityLikes.generateNonEmptyList().toList

      val Right(graph) = DefaultGraph.from(JsonLD.arr(array1Entities: _*), JsonLD.arr(array2Entities: _*))

      graph.entities shouldBe array1Entities ::: array2Entities
    }

    "allow instantiating from a JsonLDEntityLike objects" in {
      val entity1 = jsonLDEntityLikes.generateOne
      val entity2 = jsonLDEntityLikes.generateOne

      val Right(graph) = DefaultGraph.from(entity1, entity2)

      graph.entities shouldBe entity1 :: entity2 :: Nil
    }

    "allow instantiating from a nested JsonLDArrays of JsonLDEntityLike objects" in {
      val entities = jsonLDEntityLikes.generateNonEmptyList().toList

      val Right(graph) = DefaultGraph.from(JsonLD.arr(JsonLD.arr(entities: _*)))

      graph.entities shouldBe entities
    }

    "return a failure if there are objects other than JsonLDEntityLike" in {
      val entities = jsonLDEntityLikes.generateNonEmptyList().toList
      val other    = jsonLDValues.generateNonEmptyList().toList

      val Left(error) = DefaultGraph.from(JsonLD.arr(entities ::: other: _*))

      error.getMessage shouldBe "DefaultGraph can be instantiated with a Entities and Edges only"
    }
  }

  "fromJsonLDsUnsafe" should {

    "instantiate successfully from JsonLDEntityLike objects" in {
      val entities: NonEmptyList[JsonLD] = jsonLDEntityLikes.generateNonEmptyList()

      val graph = DefaultGraph.fromJsonLDsUnsafe(entities.head, entities.tail: _*)

      graph.entities shouldBe entities.toList
    }

    "throw a failure if there are objects other than JsonLDEntityLike" in {
      val entities: NonEmptyList[JsonLD] = jsonLDEntityLikes.generateNonEmptyList() :+ jsonLDValues.generateOne

      intercept[IllegalArgumentException] {
        DefaultGraph.fromJsonLDsUnsafe(entities.head, entities.tail: _*)
      }.getMessage shouldBe "DefaultGraph can be instantiated with a Entities and Edges only"
    }
  }

  "toJson" should {

    "turn the given DefaultGraph into JSON" in {
      forAll { (graph: DefaultGraph) =>
        graph.toJson shouldBe json"""{
          "@graph": ${graph.entities.map(_.toJson)}
        }"""
      }
    }
  }

  "entityId" should {

    "return None" in {
      DefaultGraph(jsonLDEntities.generateNonEmptyList().toList).entityId shouldBe None
    }
  }

  "entityTypes" should {

    "return None" in {
      defaultGraphs.generateOne.entityTypes shouldBe None
    }
  }

  "asArray" should {

    "return a single element Array with itself" in {
      val graph = defaultGraphs.generateOne

      graph.asArray shouldBe Some(Vector(graph))
    }
  }

  "merge" should {

    "return the same DefaultGraph if it contains only one entity" in {
      val graph = DefaultGraph(jsonLDEntities.generateOne)

      graph.merge shouldBe graph.asRight
    }

    "return DefaultGraph with merged entities in the sense of an JsonLDArray merge " +
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

        val Right(mergedDefaultGraph) = DefaultGraph(entity1, entity2, edge).merge

        val Right(merged) = JsonLD.arr(entity1, entity2, edge).merge
        mergedDefaultGraph.entities shouldBe merged.asArray.sequence.flatten
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

    "return the same DefaultGraph if it contains only one not nested entity" in {
      val graph = DefaultGraph(jsonLDEntities.generateOne)

      graph.flatten shouldBe graph.asRight
    }

    "return DefaultGraph with flattened entities in the sense of an JsonLDArray merge " +
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

            val Right(flattened) = DefaultGraph(grandparentWithChild, otherEntity, edge).flatten

            flattened.entities should contain theSameElementsAs
              flattenedGrandparent :: flattenedParent :: children ::: otherEntity :: edge :: Nil
        }
      }
  }
}
