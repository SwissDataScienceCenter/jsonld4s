package examples

import examples.Flattening.schema
import io.renku.jsonld.{EntityTypes, JsonLD, JsonLDDecoder}
import io.renku.jsonld.parser._

object BasicDecoding {

  val input = """                  
                |[
                |  {
                |    "@id" : "http://example.org/projects/1420319887",
                |    "@type" : "http://schema.org/Project",
                |    "http://schema.org/member" : [
                |      {
                |        "@id" : "http://example.org/users/-296456315"
                |      },
                |      {
                |        "@id" : "http://example.org/users/51770787"
                |      }
                |    ],
                |    "http://schema.org/name" : {
                |      "@value" : "MyProject"
                |    }
                |  },
                |  {
                |    "@id" : "http://example.org/users/-296456315",
                |    "@type" : "http://schema.org/Project",
                |    "http://schema.org/name" : {
                |      "@value" : "User1"
                |    }
                |  },
                |  {
                |    "@id" : "http://example.org/users/51770787",
                |    "@type" : "http://schema.org/Project",
                |    "http://schema.org/name" : {
                |      "@value" : "User2"
                |    }
                |  },
                |  {
                |    "@id" : "http://example.org/users/704621988",
                |    "@type" : "http://schema.org/Project",
                |    "http://schema.org/name" : {
                |      "@value" : "User1"
                |    }
                |  },
                |  {
                |    "@id" : "http://example.org/users/1306141351",
                |    "@type" : "http://schema.org/Project",
                |    "http://schema.org/name" : {
                |      "@value" : "User2"
                |    }
                |  }
                |]
                |""".stripMargin

  val result: Either[ParsingFailure, JsonLD] = parse(input)

  implicit val userDecoder:    JsonLDDecoder[User]    = ???
  implicit val projectDecoder: JsonLDDecoder[Project] = ???
  result.map((json: JsonLD) => json.cursor.as[List[Project]])

  // SHOW OTHER TYPES OUT OF THE BOX
  // OPTIONAL FIELDS - DECODING / ENCODING
  //
  // SETS, LISTS, SEQUENCES

}
