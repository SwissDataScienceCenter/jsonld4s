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

package examples

import ExampleSchemas._
import io.renku.jsonld._
import io.circe.literal._
import io.renku.jsonld.parser._
import org.scalatest.wordspec.AnyWordSpec

class ConditionalDecoding extends AnyWordSpec {

  private implicit val conditionalUserDecoder: JsonLDEntityDecoder[User] = JsonLDDecoder.entity(
    EntityTypes of (schema / "Person"),
    predicate = _.downField(schema / "name").as[String].map(_ == "User2")
  )(cursor => cursor.downField(schema / "name").as[String].map(User))

  private val input = json"""
    [
      {
        "@id" : "http://example.org/users/704621988",
        "@type" : "http://schema.org/Person",
        "http://schema.org/name" : {
          "@value" : "User1"
        }
      },
      {
        "@id" : "http://example.org/users/12321123",
        "@type" : "http://schema.org/Person",
        "http://schema.org/name" : {
          "@value" : "User2"
        }
      }
    ]
  """.spaces2

  private val result: Either[ParsingFailure, JsonLD] = parse(input)

  assert(result.flatMap(jsonldOutput => jsonldOutput.cursor.as[List[User]]) == Right(List(User("User2"))))
}
