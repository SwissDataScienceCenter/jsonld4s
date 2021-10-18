package examples

import examples.ExampleSchemas.schema
import io.circe.literal._
import io.renku.jsonld.parser.{ParsingFailure, parse}
import io.renku.jsonld.syntax._
import io.renku.jsonld.{EntityId, EntityTypes, JsonLD, JsonLDDecoder, JsonLDEncoder}

object OptionalFields extends App {

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

  private val expectedJson =
    json"""
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

  private val input =
    """
      |[
      |  {
      |    "@id" : "http://entity/63476538",
      |    "@type" : "http://schema.org/Organization",
      |    "http://schema.org/legalName" : {
      |      "@value" : "Apple Inc."
      |    },
      |    "http://schema.org/name" : {
      |      "@value" : "Apple"
      |    }
      |  },
      |  {
      |    "@id" : "http://entity/1512622644",
      |    "@type" : "http://schema.org/Organization",
      |    "http://schema.org/name" : {
      |      "@value" : "Applebee's"
      |    }
      |  }
      |]
      |""".stripMargin

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
