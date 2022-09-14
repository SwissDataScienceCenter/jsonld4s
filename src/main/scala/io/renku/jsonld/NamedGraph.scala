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

import cats.data.Chain
import cats.syntax.all._
import io.renku.jsonld.JsonLD.{JsonLDArray, JsonLDEntityLike, MalformedJsonLD}
import io.renku.jsonld.flatten.{DefaultGraphFlatten, JsonLDFlatten, NamedGraphFlatten}
import io.renku.jsonld.merge.EntitiesMerger
import GraphOps._

import scala.annotation.tailrec

sealed trait Graph extends JsonLD with JsonLDFlatten {
  val entities: Seq[JsonLDEntityLike]
  def flatten: Either[MalformedJsonLD, Graph]
}

final case class NamedGraph(id: EntityId, entities: Seq[JsonLDEntityLike])
    extends Graph
    with NamedGraphFlatten
    with EntitiesMerger {

  import io.circe.Json
  import io.circe.literal._

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

  def from(id: EntityId, jsonLD: JsonLD, otherJsonLDs: JsonLD*): Either[IllegalArgumentException, NamedGraph] =
    flatten(Chain.fromSeq(jsonLD :: otherJsonLDs.toList), Chain.empty).map(c => NamedGraph(id, c.toList))

  def fromJsonLDsUnsafe(id: EntityId, jsonLD: JsonLD, otherJsonLDs: JsonLD*): NamedGraph =
    from(id, jsonLD, otherJsonLDs: _*).fold(throw _, identity)
}

final case class DefaultGraph(entities: Seq[JsonLDEntityLike])
    extends Graph
    with DefaultGraphFlatten
    with EntitiesMerger {

  import io.circe.Json
  import io.circe.literal._

  override lazy val toJson: Json = json"""{
    "@graph": ${entities.map(_.toJson)}
  }"""

  override lazy val entityId: Option[EntityId] = None

  override lazy val entityTypes: Option[EntityTypes] = None

  override lazy val asArray: Option[Vector[JsonLD]] = Some(Vector(this))

  override lazy val merge: Either[JsonLD.MalformedJsonLD, DefaultGraph] =
    this.copy(entities = mergeEntities(entities).toList).asRight
}

object DefaultGraph {

  def apply(entity: JsonLDEntityLike, otherEntities: JsonLDEntityLike*): DefaultGraph =
    DefaultGraph(entity :: otherEntities.toList)

  def from(jsonLD: JsonLD, otherJsonLDs: JsonLD*): Either[IllegalArgumentException, DefaultGraph] =
    flatten(Chain.fromSeq(jsonLD :: otherJsonLDs.toList), Chain.empty).map(c => DefaultGraph(c.toList))

  def fromJsonLDsUnsafe(jsonLD: JsonLD, otherJsonLDs: JsonLD*): DefaultGraph =
    from(jsonLD, otherJsonLDs: _*).fold(throw _, identity)
}

private object GraphOps {

  @tailrec
  final def flatten(toFlatten: Chain[JsonLD],
                    flattened: Chain[JsonLDEntityLike]
  ): Either[IllegalArgumentException, Chain[JsonLDEntityLike]] = toFlatten.uncons match {
    case None                                 => flattened.asRight
    case Some((json: JsonLDEntityLike, more)) => flatten(more, flattened :+ json)
    case Some((JsonLDArray(jsons), more))     => flatten(Chain.fromSeq(jsons) concat more, flattened)
    case _ => new IllegalArgumentException("DefaultGraph can be instantiated with a Entities and Edges only").asLeft
  }
}
