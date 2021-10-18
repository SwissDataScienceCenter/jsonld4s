package examples

import examples.ExampleSchemas.prov
import io.renku.jsonld._
import io.renku.jsonld.syntax._
import io.circe.literal._

object BasicEncoding extends App {

  final case class MyType(value: String)

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
