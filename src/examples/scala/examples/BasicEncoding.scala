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

package examples

import examples.ExampleSchemas.prov
import io.renku.jsonld._
import io.renku.jsonld.syntax._
import io.circe.literal._
import org.scalatest.wordspec.AnyWordSpec

class BasicEncoding extends AnyWordSpec {

  case class MyType(value: String)

  private implicit val myTypeEncoder: JsonLDEncoder[MyType] = JsonLDEncoder.instance { entity =>
    JsonLD.entity(
      EntityId of "http://entity/23424",
      EntityTypes of (prov / "Entity"),
      prov / "value" -> entity.value.asJsonLD
    )
  }

  private val expectedOutput =
    json"""{
             "@id" : "http://entity/23424",
             "@type" : "http://www.w3.org/ns/prov#Entity",
             "http://www.w3.org/ns/prov#value" : {
               "@value" : "some value"
             }
           }
           """

  assert(MyType(value = "some value").asJsonLD.toJson == expectedOutput)
}
