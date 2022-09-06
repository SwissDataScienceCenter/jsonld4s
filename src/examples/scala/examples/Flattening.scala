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

import examples.ExampleSchemas.schema
import io.circe.literal._
import io.renku.jsonld._
import io.renku.jsonld.syntax._
import org.scalatest.wordspec.AnyWordSpec

class Flattening extends AnyWordSpec {

  // Encoding

  private implicit val userEncoder: JsonLDEncoder[User] = JsonLDEncoder.instance { user =>
    JsonLD.entity(
      EntityId of s"http://example.org/users/${user.name.hashCode}",
      EntityTypes of (schema / "Person"),
      schema / "name" -> user.name.asJsonLD
    )
  }

  implicit val projectEncoder: JsonLDEncoder[Project] = JsonLDEncoder.instance { entity =>
    JsonLD.entity(
      EntityId of s"http://example.org/projects/${project.name.hashCode}",
      EntityTypes of (schema / "Project"),
      schema / "name"   -> entity.name.asJsonLD,
      schema / "member" -> entity.members.asJsonLD
    )
  }

  private val members = List(User("User1"), User("User2"))
  private val project = Project(name = "MyProject", members)

  private val expectedJsonTreeFull = json"""{
    "@id" : "http://example.org/projects/46955437",
    "@type" : "http://schema.org/Project",
    "http://schema.org/member" : [
      {
        "@id" : "http://example.org/users/82025894",
        "@type" : "http://schema.org/Person",
        "http://schema.org/name" : {
          "@value" : "User1"
        }
      },
      {
        "@id" : "http://example.org/users/82025895",
        "@type" : "http://schema.org/Person",
        "http://schema.org/name" : {
          "@value" : "User2"
        }
      }
    ],
    "http://schema.org/name" : {
      "@value" : "MyProject"
    }
  }"""

  assert(project.asJsonLD.toJson == expectedJsonTreeFull)

  private val expectedJsonFlattened = json"""
    [
      {
        "@id" : "http://example.org/projects/46955437",
        "@type" : "http://schema.org/Project",
        "http://schema.org/member" : [
          {
            "@id" : "http://example.org/users/82025894"
          },
          {
            "@id" : "http://example.org/users/82025895"
          }
        ],
        "http://schema.org/name" : {
          "@value" : "MyProject"
        }
      },
      {
        "@id" : "http://example.org/users/82025894",
        "@type" : "http://schema.org/Person",
        "http://schema.org/name" : {
          "@value" : "User1"
        }
      },
      {
        "@id" : "http://example.org/users/82025895",
        "@type" : "http://schema.org/Person",
        "http://schema.org/name" : {
          "@value" : "User2"
        }
      }
    ]"""

  assert(project.asJsonLD.flatten.fold(throw _, identity).toJson == expectedJsonFlattened)

  // Decoding

  private val userEntityTypes:    EntityTypes = EntityTypes.of(schema / "Person")
  private val projectEntityTypes: EntityTypes = EntityTypes.of(schema / "Project")

  private implicit val userDecoder: JsonLDDecoder[User] = JsonLDDecoder.entity(userEntityTypes) { cursor =>
    cursor.downField(schema / "name").as[String].map(name => User(name))
  }

  private implicit val projectDecoder: JsonLDDecoder[Project] = JsonLDDecoder.entity(projectEntityTypes) { cursor =>
    for {
      name    <- cursor.downField(schema / "name").as[String]
      members <- cursor.downField(schema / "member").as[List[User]]
    } yield Project(name, members)
  }

  assert(project.asJsonLD.flatten.flatMap(_.cursor.as[List[Project]]) == Right(List(project)))
}
