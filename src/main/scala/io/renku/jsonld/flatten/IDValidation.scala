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

package io.renku.jsonld.flatten

import io.renku.jsonld.{EntityId, JsonLD}
import io.renku.jsonld.JsonLD.{JsonLDEntity, MalformedJsonLD}

private object IDValidation {

  def checkForUniqueIds[T <: JsonLD](flattenedJsons: List[T]): Either[MalformedJsonLD, List[T]] =
    findNotEqualEntities(flattenedJsons) match {
      case Nil      => Right(flattenedJsons)
      case entities => Left(MalformedJsonLD(s"Not equal entity(ies) in json-ld: ${entities.mkString(",")}"))
    }

  private def findNotEqualEntities(jsons: List[JsonLD]): List[EntityId] =
    jsons
      .collect { case entity: JsonLDEntity => entity }
      .groupBy(entity => entity.id)
      .collect { case (entityId, entitiesPerId) if entitiesPerId.exists(_ != entitiesPerId.head) => entityId }
      .toList
}
