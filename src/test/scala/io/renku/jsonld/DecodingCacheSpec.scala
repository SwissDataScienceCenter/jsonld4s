/*
 * Copyright 2024 Swiss Data Science Center (SDSC)
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
import io.circe.DecodingFailure
import io.renku.jsonld.generators.Generators.Implicits._
import io.renku.jsonld.generators.JsonLDGenerators._
import org.scalacheck.Arbitrary
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DecodingCacheSpec extends AnyWordSpec with should.Matchers with ScalaCheckPropertyChecks {

  "get and put" should {

    "cache the value for a CacheableEntityDecoder" in new TestCase {
      forAll { (entityId: EntityId, obj: Int) =>
        implicit val cacheableDecoder: CacheableEntityDecoder = newCacheableDecoder()

        cache.put(entityId, obj) shouldBe obj

        cache.get(entityId) shouldBe Some(obj)
      }
    }

    "do not cache the value for a non-CacheableEntityDecoder" in new TestCase {
      forAll { (entityId: EntityId, obj: Int) =>
        implicit val nonCacheableDecoder: CacheableEntityDecoder = newNonCacheableDecoder()

        cache.put(entityId, obj) shouldBe obj

        cache.get(entityId) shouldBe None
      }
    }

    "do not mix-up values having the same entityId but different decoders" in new TestCase {

      val entityId = entityIds.generateOne

      val cacheableDecoder1: CacheableEntityDecoder = newCacheableDecoder()
      val obj1 = Arbitrary.arbInt.arbitrary.generateOne

      val cacheableDecoder2: CacheableEntityDecoder = newCacheableDecoder()
      val obj2 = Arbitrary.arbInt.arbitrary.generateOne

      val nonCacheableDecoder: CacheableEntityDecoder = newNonCacheableDecoder()
      val nonCachedObj = Arbitrary.arbInt.arbitrary.generateOne

      cache.put(entityId, obj1)(cacheableDecoder1)           shouldBe obj1
      cache.put(entityId, obj2)(cacheableDecoder2)           shouldBe obj2
      cache.put(entityId, nonCachedObj)(nonCacheableDecoder) shouldBe nonCachedObj

      cache.get(entityId)(cacheableDecoder1)   shouldBe Some(obj1)
      cache.get(entityId)(cacheableDecoder2)   shouldBe Some(obj2)
      cache.get(entityId)(nonCacheableDecoder) shouldBe None
    }
  }

  private trait TestCase {
    val cache = DecodingCache.empty
  }

  private def newCacheableDecoder() = JsonLDDecoder
    .cacheableEntity[Int](entityTypesObject.generateOne)(_ =>
      Arbitrary.arbInt.arbitrary.generateOne.asRight[DecodingFailure]
    )
    .cacheableDecoder

  private def newNonCacheableDecoder() = JsonLDDecoder
    .entity[Int](entityTypesObject.generateOne)(_ => Arbitrary.arbInt.arbitrary.generateOne.asRight[DecodingFailure])
    .cacheableDecoder
}
