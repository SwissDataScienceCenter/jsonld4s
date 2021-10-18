package examples

import io.renku.jsonld.Schema

object ExampleSchemas {
  val schema: Schema = Schema.from("http://schema.org")
  val prov:   Schema = Schema.from("http://www.w3.org/ns/prov", separator = "#")
}
