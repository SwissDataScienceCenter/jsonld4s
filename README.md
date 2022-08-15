<meta name='keywords' content='scala, json-ld'>

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

#### Conditional decoding

jsold4s allows defining predicates on Entity Decoders. Predicates along Entity Types work as filters in the decoding process so if there's an Entity with correct type but condition encoded in the predicate is not met on it, the entity will be skipped. An example where predicates come handy might be situation of multiple implementation of a type where each implementation differs in properties or property value.

Predicates are functions of type `Cursor => JsonLDDecoder.Result[Boolean]` to give user great flexibility. An example of a predicate verifying if a `schema:name` property on an Entity matches the value might be:

```scala
val predicate: Cursor => JsonLDDecoder.Result[Boolean] =
  _.downField(schema / "name").as[String].map(_ == "some arbitrary name")
```

For more details see: `src/examples/scala/examples/ConditionalDecoding.scala`

#### Cacheable entities

Decoding big json-ld payloads into model classes may sometimes result in less satisfying performance. The library gives a tool for specifying, so called `Cacheable Entities`. Entities marked as cacheable are put into internal cache when decoded for the first time. Later, when the same entity is found in other places in the model hierarchy, the decoded instance is taken from the cache rather than being decoded again. Marking an Entity as cacheable might give benefits, however, the feature has to be used wisely as adding all entities to the cache, even if they occur in the payload only once, may lead to performance which is even worse than if there's no cache at all. The reason for that is the size of the cache. If size of the cache grows, the cache simply becomes slower. So a reasonable approach is to mark cacheable only these Entities which instances either occur many times in the payload or at least twice but they are very costly to decode.

Defining a cacheable entity is simple. It requires `JsonLDDecoder.cacheableEntity` instead of `JsonLDDecoder.entity`. The signatures of both factories are the same.

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

## Ontology

JsonLD4s library allows defining and generating ontologies.

### Defining ontology

Defining ontology of a specific type should be done using the `io.renku.jsonld.ontology.Type` class. An example definition can look like follows:

```scala
import io.renku.jsonld.ontology._
import io.renku.jsonld.Schema

val prov:   Schema = Schema.from("http://www.w3.org/ns/prov", separator = "#")
val renku:  Schema = Schema.from("https://swissdatasciencecenter.github.io/renku-ontology", separator = "#")
val schema: Schema = Schema.from("http://schema.org")

val subtypeOntology: Type = Type.Def(
  Class(schema / "Thing"),
  DataProperty(schema / "name", xsd / "string")
)

val rootOntology: Type = Type.Def(
  Class(prov / "Activity"),
  ObjectProperties(
    ObjectProperty(renku / "parameter", subtypeOntology)
  ),
  DataProperties(DataProperty(prov / "startedAtTime", xsd / "dateTime"),
                 DataProperty(prov / "endedAtTime", xsd / "dateTime")
  )
)
```

Type's class needs to be defined with the `Class` type, properties linking other types with a collection of `ObjectProperty` objects and simple value properties with a collection of `DataProperty` objects. The library calculates properties' ranges and domains automatically during ontology generation.

### Generating ontology

Generating ontology is a trivial task which can be done using the `io.renku.jsonld.ontology.generateOntology` method. The method takes a `Type` definition and a `Schema`.

```scala
import io.renku.jsonld.ontology._
import io.renku.jsonld.Schema

val renku:  Schema = Schema.from("https://swissdatasciencecenter.github.io/renku-ontology", separator = "#")
val schema: Schema = Schema.from("http://schema.org")

val ontology: Type = Type.Def(
  Class(schema / "Thing"),
  DataProperty(schema / "name", xsd / "string")
)

generateOntology(ontology, renku)
```