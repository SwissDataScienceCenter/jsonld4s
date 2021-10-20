# json-ld

This is a Scala library to work with Json-LD. It's build on top of [Circe](https://circe.github.io/circe) and follows
the design choices of its API.

## Encoding to Json-LD

The library allows to encode any Scala object to Json-LD. Encoders for common types like `String`, `Int`, `Long`
, `java.time.Instant` and `java.time.LocalDate` are provided by the library. There are also facilities for encoding
objects, arrays of objects, options, lists and sets. All the mentioned tools exists in the `io.renku.jsonld.JsonLD`
object.

Example:

```
import io.renku.jsonld._

JsonLD.fromInt(1)
JsonLD.fromOption(Some("abc"))
JsonLD.arr(JsonLD.fromString("a"), JsonLD.fromString("b"), JsonLD.fromString("c"))
JsonLD.entity(
  EntityId of "http://entity/23424",
  EntityTypes of (Schema.from("http://schema.org") / "Project"),
  Schema.from("http://schema.org") / "name" -> JsonLD.fromString("value")
)
```

Improved readability can be achieved by providing encoders for the types together with implicits from
the `io.renku.jsonld.syntax` package.

Example:

```
import io.renku.jsonld._
import io.renku.jsonld.syntax._

val schema: Schema = Schema.from("http://schema.org")

final case class MyType(name: String)

implicit val myTypeEncoder: JsonLDEncoder[MyType] = JsonLDEncoder.instance { entity =>
    JsonLD.entity(
      EntityId of "http://entity/23424",
      EntityTypes of (schema / "Project"),
      schema / "name" -> entity.name.asJsonLD
    )
  }
  
 MyType(name = "some name").asJsonLD
```

## Decoding

Decoding is an operation which allows extraction of an object from JSON-LD.

**Note: When you decode from flattened JSON-LD, you have to decode to a list of your object type.**

```scala
import examples.ExampleSchemas.schema
import io.renku.jsonld.syntax._
import io.renku.jsonld.{EntityId, EntityTypes, JsonLD, JsonLDDecoder}

private implicit val userDecoder: JsonLDDecoder[User] = JsonLDDecoder.entity(EntityTypes.of(schema / "Person")) {
  cursor =>
    cursor.downField(schema / "name").as[String].map(name => User(name))
}

JsonLD
  .entity(EntityId of "https://example.com/1234",
    EntityTypes of schema / "Person",
    schema / "name" -> "Angela".asJsonLD
  )
  .cursor
  .as[User]
```

Conditional decoding is also supported. For details see `src/examples/scala/examples/ConditionalDecoding.scala`

## Parsing

Allows for parsing from circe-json or a string to a JsonLD object. See `src/examples/scala/examples/BasicDecoding.scala`
.

## Flattening

Flattening allows for data de-nesting in order to compress the data and have all entities in a single array. A failure
is returned if run on JSON-LD containing multiple entities with the same `@id` but different content.

```scala
val nestedJson = json"""..."""
nestedJson.asJsonLD.flatten
```

Nested Json

```json
{
  "@id": "http://example.org/projects/46955437",
  "@type": "http://schema.org/Project",
  "http://schema.org/member": [
    {
      "@id": "http://example.org/users/82025894",
      "@type": "http://schema.org/Person",
      "http://schema.org/name": {
        "@value": "User1"
      }
    }
  ],
  "http://schema.org/name": {
    "@value": "MyProject"
  }
}
```

Flattened Json

```json
[
  {
    "@id": "http://example.org/projects/46955437",
    "@type": "http://schema.org/Project",
    "http://schema.org/member": [
      {
        "@id": "http://example.org/users/82025894"
      }
    ],
    "http://schema.org/name": {
      "@value": "MyProject"
    }
  },
  {
    "@id": "http://example.org/users/82025894",
    "@type": "http://schema.org/Person",
    "http://schema.org/name": {
      "@value": "User1"
    }
  }
]
```

## Supported types

The following types are supported for both encoding and decoding:

- String
- Int
- Long
- Instant
- LocalDate
- EntityId
- JsonLD
- Boolean
- List
- Set
- Seq
- Option

## Encoding to Json

Every JsonLD object can be turn to Json using it's `toJson` method.

## Detailed examples

**For More examples, please see src/examples/scala/examples**
