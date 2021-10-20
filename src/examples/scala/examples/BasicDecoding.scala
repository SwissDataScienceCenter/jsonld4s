package examples

import examples.ExampleSchemas.schema
import io.renku.jsonld.parser._
import io.renku.jsonld.{EntityTypes, JsonLD, JsonLDDecoder}

object BasicDecoding extends App {

  private val input: String =
    """
      |[
      |  {
      |    "@id" : "http://example.org/projects/46955437",
      |    "@type" : "http://schema.org/Project",
      |    "http://schema.org/member" : [
      |      {
      |        "@id" : "http://example.org/users/82025894",
      |        "@type" : "http://schema.org/Person",
      |        "http://schema.org/name" : {
      |          "@value" : "User1"
      |        }
      |      },
      |      {
      |        "@id" : "http://example.org/users/82025895",
      |        "@type" : "http://schema.org/Person",
      |        "http://schema.org/name" : {
      |          "@value" : "User2"
      |        }
      |      }
      |    ],
      |    "http://schema.org/name" : {
      |      "@value" : "MyProject"
      |    }
      |  }
      |]
      |""".stripMargin

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

  private val result: Either[ParsingFailure, JsonLD] = parse(input)
  assert(
    result.flatMap((json: JsonLD) => (json.cursor.as[List[Project]])) == Right(
      List(Project("MyProject", List(User("User1"), User("User2"))))
    )
  )

}
