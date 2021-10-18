package examples
import ExampleSchemas._
import io.renku.jsonld.parser._
import io.renku.jsonld.{Cursor, EntityTypes, JsonLD, JsonLDDecoder, JsonLDEntityDecoder}

object ConditionalDecoding extends App {

  private implicit val conditionalUserDecoder: JsonLDEntityDecoder[User] = JsonLDDecoder.entity(
    EntityTypes of (schema / "Person"),
    predicate = _.downField(schema / "name").as[String].map(_ == "User2")
  )(cursor => cursor.downField(schema / "name").as[String].map(User(_)))

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
