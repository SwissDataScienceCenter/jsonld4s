package examples

import examples.Flattening.schema
import io.renku.jsonld.{EntityTypes, JsonLDDecoder}

object ConditionalDecoding extends App {
  val conditionalUserDecoder = JsonLDDecoder.entity(
    EntityTypes of (schema / "Person"),
    cursor => cursor.downField(schema / "name").as[String].map(_ == "User2")
  )(cursor => cursor.downField(schema / "name").as[String].map(User(_)))

}
