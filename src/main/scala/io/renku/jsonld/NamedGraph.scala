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

package io.renku.jsonld

import cats.syntax.all._
import io.renku.jsonld.JsonLD.JsonLDEntityLike
import io.renku.jsonld.flatten.NamedGraphFlatten
import io.renku.jsonld.merge.EntitiesMerger

final case class NamedGraph(id: EntityId, entities: List[JsonLDEntityLike])
    extends JsonLD
    with NamedGraphFlatten
    with EntitiesMerger {

  import io.circe.Json
  import io.circe.literal._

  override type T = NamedGraph

  override lazy val toJson: Json = json"""{
    "@id":    ${id.asJson},
    "@graph": ${entities.map(_.toJson)}
  }"""

  override lazy val entityId: Option[EntityId] = Some(id)

  override lazy val entityTypes: Option[EntityTypes] = None

  override lazy val asArray: Option[Vector[JsonLD]] = Some(Vector(this))

  override lazy val merge: Either[JsonLD.MalformedJsonLD, NamedGraph] =
    this.copy(entities = mergeEntities(entities).toList).asRight
}

object NamedGraph {

  def apply(id: EntityId, entity: JsonLDEntityLike, otherEntities: JsonLDEntityLike*): NamedGraph =
    NamedGraph(id, entity :: otherEntities.toList)

}
