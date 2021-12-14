/*
 * Copyright 2021 Swiss Data Science Center (SDSC)
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

trait DecodingCache {
  def get[A](entityId: EntityId)(implicit cacheableDecoder: CacheableEntityDecoder[A]): Option[A]
  def offer[A](entityId: EntityId, obj: A)(implicit cacheableDecoder: CacheableEntityDecoder[A]): A
}

object DecodingCache {

  val empty: DecodingCache = new ByEntityId()

  private class ByEntityId() extends DecodingCache {

    import collection.mutable

    private case class CacheKey(entityId: EntityId, decoder: CacheableEntityDecoder.Yes[_])

    private val cache: mutable.Map[CacheKey, Any] = mutable.Map.empty[CacheKey, Any]

    override def get[A](entityId: EntityId)(implicit cacheableDecoder: CacheableEntityDecoder[A]): Option[A] =
      cacheableDecoder match {
        case decoder: CacheableEntityDecoder.Yes[A] => cache.get(CacheKey(entityId, decoder)).map(_.asInstanceOf[A])
        case _:       CacheableEntityDecoder.No[A]  => None
      }

    override def offer[A](entityId: EntityId, obj: A)(implicit cacheableDecoder: CacheableEntityDecoder[A]): A =
      cacheableDecoder match {
        case decoder: CacheableEntityDecoder.Yes[A] =>
          cache.addOne(CacheKey(entityId, decoder) -> obj)
          obj
        case _: CacheableEntityDecoder.No[A] => obj
      }
  }
}
