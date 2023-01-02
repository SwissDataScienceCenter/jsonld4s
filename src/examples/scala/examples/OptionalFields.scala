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

import examples.ExampleSchemas.schema
import io.circe.literal._
import io.renku.jsonld.parser.{ParsingFailure, parse}
import io.renku.jsonld.syntax._
import io.renku.jsonld.{EntityId, EntityTypes, JsonLD, JsonLDDecoder, JsonLDEncoder}
import org.scalatest.wordspec.AnyWordSpec

class OptionalFields extends AnyWordSpec {

  // Encoding

  private implicit val organizationEncoder: JsonLDEncoder[Organization] = JsonLDEncoder.instance { organization =>
    JsonLD.entity(
      EntityId of s"http://entity/${organization.name.hashCode}",
      EntityTypes of (schema / "Organization"),
      schema / "name"      -> organization.name.asJsonLD,
      schema / "legalName" -> organization.legalName.asJsonLD
    )
  }

  private val apple         = Organization("Apple", Some("Apple Inc."))
  private val applebees     = Organization("Applebee's", legalName = None)
  private val organizations = JsonLD.arr(apple.asJsonLD, applebees.asJsonLD)

  private val expectedJson = json"""
    [
      {
        "@id" : "http://entity/63476538",
        "@type" : "http://schema.org/Organization",
        "http://schema.org/legalName" : {
          "@value" : "Apple Inc."
        },
        "http://schema.org/name" : {
          "@value" : "Apple"
        }
      },
      {
        "@id" : "http://entity/1512622644",
        "@type" : "http://schema.org/Organization",
        "http://schema.org/name" : {
          "@value" : "Applebee's"
        }
      }
    ]
  """

  assert(organizations.toJson == expectedJson)

  // Decoding

  private val input = json"""
    [
      {
        "@id" : "http://entity/63476538",
        "@type" : "http://schema.org/Organization",
        "http://schema.org/legalName" : {
          "@value" : "Apple Inc."
        },
        "http://schema.org/name" : {
          "@value" : "Apple"
        }
      },
      {
        "@id" : "http://entity/1512622644",
        "@type" : "http://schema.org/Organization",
        "http://schema.org/name" : {
          "@value" : "Applebee's"
        }
      }
    ]
  """.spaces2

  private val entityTypes: EntityTypes = EntityTypes.of(schema / "Organization")

  private implicit val organizationDecoder: JsonLDDecoder[Organization] = JsonLDDecoder.entity(entityTypes) { cursor =>
    for {
      name      <- cursor.downField(schema / "name").as[String]
      legalName <- cursor.downField(schema / "legalName").as[Option[String]]
    } yield Organization(name, legalName)
  }

  private val result: Either[ParsingFailure, JsonLD] = parse(input)

  assert(result.flatMap((json: JsonLD) => json.cursor.as[List[Organization]]) == Right(List(apple, applebees)))
}
