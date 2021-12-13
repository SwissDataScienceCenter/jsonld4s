package examples

import ExampleSchemas._
import io.renku.jsonld._
import io.renku.jsonld.parser._
import org.scalatest.wordspec.AnyWordSpec

class ConditionalDecoding extends AnyWordSpec {

  private implicit val conditionalUserDecoder: JsonLDEntityDecoder[User] = JsonLDDecoder.entity(
    EntityTypes of (schema / "Person"),
    predicate = _.downField(schema / "name").as[String].map(_ == "User2")
  )(cursor => cursor.downField(schema / "name").as[String].map(User))

  private val input =
    s"""
       |[
       |  {
       |    "@id" : "http://example.org/users/704621988",
       |    "@type" : "http://schema.org/Person",
       |    "http://schema.org/name" : {
       |      "@value" : "User1"
       |    }
       |  },
       |  {
       |    "@id" : "http://example.org/users/12321123",
       |    "@type" : "http://schema.org/Person",
       |    "http://schema.org/name" : {
       |      "@value" : "User2"
       |    }
       |  }
       |]
       |""".stripMargin

  private val result: Either[ParsingFailure, JsonLD] = parse(input)

  assert(result.flatMap(jsonldOutput => jsonldOutput.cursor.as[List[User]]) == Right(List(User("User2"))))

}
