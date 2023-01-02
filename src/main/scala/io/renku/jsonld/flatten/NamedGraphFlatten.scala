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

package io.renku.jsonld.flatten

import cats.syntax.all._
import io.renku.jsonld.JsonLD._
import io.renku.jsonld.merge.EntitiesMerger
import io.renku.jsonld.{DefaultGraph, Graph, NamedGraph}

trait NamedGraphFlatten extends JsonLDFlatten with GraphFlatten {
  self: NamedGraph =>

  override lazy val flatten: Either[MalformedJsonLD, NamedGraph] =
    doFlattening.map(flattened => this.copy(entities = flattened))
}

trait DefaultGraphFlatten extends JsonLDFlatten with GraphFlatten {
  self: DefaultGraph =>

  override lazy val flatten: Either[MalformedJsonLD, DefaultGraph] =
    doFlattening.map(flattened => this.copy(entities = flattened))
}

trait GraphFlatten extends EntitiesMerger {
  self: Graph =>

  import Flatten._
  import IDValidation._

  protected lazy val doFlattening: Either[MalformedJsonLD, List[JsonLDEntityLike]] = for {
    flattened <- flattenEntities
    validated <- checkForUniqueIds(flattened.distinct)
  } yield mergeEntities(validated).toList

  private def flattenEntities: Either[MalformedJsonLD, List[JsonLDEntityLike]] =
    entities.foldLeft(Either.right[MalformedJsonLD, List[JsonLDEntityLike]](List.empty[JsonLDEntityLike])) {
      case (flattened, entity: JsonLDEntity) => flattened >>= (deNestEntities(List(entity), _))
      case (flattened, edge: JsonLDEdge)     => flattened.map(_ ::: edge :: Nil)
    }
}
