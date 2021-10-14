package examples

import io.renku.jsonld._
import io.renku.jsonld.syntax._

import scala.util.Random

object Simple extends App {

  val schema: Schema = Schema.from("http://schema.org")

  final case class MyType(name: String)

  implicit val myTypeEncoder: JsonLDEncoder[MyType] = JsonLDEncoder.instance { entity =>
    JsonLD.entity(
      EntityId of "http://entity/23424",
      EntityTypes of (schema / "Project"),
      schema / "name" -> entity.name.asJsonLD
    )
  }

  val myJsonLDObject = MyType(name = "some name").asJsonLD

}

object Flattening extends App {

  val schema: Schema = Schema.from("http://schema.org")

  final case class User(name: String)
  final case class Project(name: String, members: List[User])

  implicit val userEncoder: JsonLDEncoder[User] = JsonLDEncoder.instance { user =>
    JsonLD.entity(
      EntityId of s"http://example.org/users/${Random.nextInt()}",
      EntityTypes of (schema / "Project"),
      schema / "name" -> user.name.asJsonLD
    )
  }

  implicit val projectEncoder: JsonLDEncoder[Project] = JsonLDEncoder.instance { entity =>
    JsonLD.entity(
      EntityId of s"http://example.org/projects/${Random.nextInt()}",
      EntityTypes of (schema / "Project"),
      schema / "name"   -> entity.name.asJsonLD,
      schema / "member" -> entity.members.asJsonLD
    )
  }

  val members = List(User("User1"), User("User2"))
  val project = Project(name = "MyProject", members)

  val allObjectsAsJsonLD = JsonLD.arr(project.asJsonLD +: members.map(_.asJsonLD): _*)

  println("Full tree: \n\n" + allObjectsAsJsonLD.toJson)
  /* Full JSON-LD tree (with duplicate data)
[
  {
    "@id" : "http://example.org/projects/1420319887",
    "@type" : "http://schema.org/Project",
    "http://schema.org/member" : [
      {
        "@id" : "http://example.org/users/-296456315",
        "@type" : "http://schema.org/Project",
        "http://schema.org/name" : {
          "@value" : "User1"
        }
      },
      {
        "@id" : "http://example.org/users/51770787",
        "@type" : "http://schema.org/Project",
        "http://schema.org/name" : {
          "@value" : "User2"
        }
      }
    ],
    "http://schema.org/name" : {
      "@value" : "MyProject"
    }
  },
  {
    "@id" : "http://example.org/users/704621988",
    "@type" : "http://schema.org/Project",
    "http://schema.org/name" : {
      "@value" : "User1"
    }
  },
  {
    "@id" : "http://example.org/users/1306141351",
    "@type" : "http://schema.org/Project",
    "http://schema.org/name" : {
      "@value" : "User2"
    }
  }
]
   */

  println("\n\nFlattened tree:\n\n" + allObjectsAsJsonLD.flatten.fold(throw _, identity).toJson)

  /* Flattened JSON-LD tree (without duplicate data)

[
  {
    "@id" : "http://example.org/projects/1420319887",
    "@type" : "http://schema.org/Project",
    "http://schema.org/member" : [
      {
        "@id" : "http://example.org/users/-296456315"
      },
      {
        "@id" : "http://example.org/users/51770787"
      }
    ],
    "http://schema.org/name" : {
      "@value" : "MyProject"
    }
  },
  {
    "@id" : "http://example.org/users/-296456315",
    "@type" : "http://schema.org/Project",
    "http://schema.org/name" : {
      "@value" : "User1"
    }
  },
  {
    "@id" : "http://example.org/users/51770787",
    "@type" : "http://schema.org/Project",
    "http://schema.org/name" : {
      "@value" : "User2"
    }
  },
  {
    "@id" : "http://example.org/users/704621988",
    "@type" : "http://schema.org/Project",
    "http://schema.org/name" : {
      "@value" : "User1"
    }
  },
  {
    "@id" : "http://example.org/users/1306141351",
    "@type" : "http://schema.org/Project",
    "http://schema.org/name" : {
      "@value" : "User2"
    }
  }
]
   */

}
