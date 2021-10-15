package examples

import io.renku.jsonld._
import io.renku.jsonld.syntax._

object BasicEncoding extends App {

  val prov: Schema = Schema.from("http://www.w3.org/ns/prov", separator = "#")

  final case class MyType(value: String)

  implicit val myTypeEncoder: JsonLDEncoder[MyType] = JsonLDEncoder.instance { entity =>
    JsonLD.entity(
      EntityId of "http://entity/23424",
      EntityTypes of (prov / "Entity"),
      prov / "value" -> entity.value.asJsonLD
    )
  }

  val myJsonLDObject = MyType(value = "some name").asJsonLD
}
