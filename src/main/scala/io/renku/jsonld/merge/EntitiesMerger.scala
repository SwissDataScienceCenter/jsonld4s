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

package io.renku.jsonld.merge

import io.renku.jsonld.JsonLD._
import io.renku.jsonld.{EntityId, JsonLD}

private[jsonld] trait EntitiesMerger {

  def mergeEntities(jsons: Seq[JsonLDEntityLike]): Seq[JsonLDEntityLike] = {
    val (edges, entities) = separateEdgesAndEntities(jsons)

    val (linkedEdges, unlinkedEdges) = edges.partition(edge => entities.exists(_.id == edge.source))

    entities.map(mergeEdges(linkedEdges.groupBy(_.source))) ::: unlinkedEdges
  }

  private lazy val separateEdgesAndEntities: Seq[JsonLDEntityLike] => (List[JsonLDEdge], List[JsonLDEntity]) =
    _.foldLeft(List.empty[JsonLDEdge] -> List.empty[JsonLDEntity]) {
      case ((edges, entities), edge: JsonLDEdge)     => (edges ::: edge :: Nil) -> entities
      case ((edges, entities), entity: JsonLDEntity) => edges                   -> (entities ::: entity :: Nil)
      case ((edges, entities), _)                    => edges                   -> entities
    }

  private def mergeEdges(edgesGrouped: Map[EntityId, List[JsonLDEdge]]): JsonLDEntity => JsonLDEntity = { entity =>
    edgesGrouped.get(entity.id).fold(entity)(mergeEdgesToEntity(entity))
  }

  private def mergeEdgesToEntity(entity: JsonLDEntity): List[JsonLDEdge] => JsonLDEntity = edges =>
    edges.foldLeft(entity) { case (mergedEntity, edge) => mergeEdgeToEntity(mergedEntity)(edge) }

  private def mergeEdgeToEntity(entity: JsonLDEntity): JsonLDEdge => JsonLDEntity = {
    case JsonLDEdge(_, property, target) =>
      val jsonLDTargetEntity = JsonLDEntityId(target)
      val updatedProperties =
        entity.properties.get(property).fold(entity.properties.updated(property, jsonLDTargetEntity)) { propertyValue =>
          val updatedProperty = updateProperty(jsonLDTargetEntity)(propertyValue)
          entity.properties.updated(property, updatedProperty)
        }
      entity.copy(properties = updatedProperties)
  }

  private def updateProperty(targetEntityId: JsonLDEntityId[EntityId]): JsonLD => JsonLD = propertyValue =>
    propertyValue.asArray.fold(JsonLD.arr(targetEntityId)) { jsonLDArr =>
      if (jsonLDArr contains targetEntityId) JsonLD.arr(jsonLDArr: _*)
      else JsonLD.arr(jsonLDArr :+ targetEntityId:                 _*)
    }
}
