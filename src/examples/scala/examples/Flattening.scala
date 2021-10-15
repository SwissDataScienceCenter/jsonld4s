package examples

import io.renku.jsonld.syntax._
import io.renku.jsonld._

object Flattening extends App {

  val schema: Schema = Schema.from("http://schema.org")

  implicit val userEncoder: JsonLDEncoder[User] = JsonLDEncoder.instance { user =>
    JsonLD.entity(
      EntityId of s"http://example.org/users/${user.name.hashCode}",
      EntityTypes of (schema / "Person"),
      schema / "name" -> user.name.asJsonLD
    )
  }

  implicit val projectEncoder: JsonLDEncoder[Project] = JsonLDEncoder.instance { entity =>
    JsonLD.entity(
      EntityId of s"http://example.org/projects/${project.name.hashCode}",
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
TODO: PASTE OUTPUT
   */

  println("\n\nFlattened tree:\n\n" + allObjectsAsJsonLD.flatten.fold(throw _, identity).toJson)

  /* Flattened JSON-LD tree (without duplicate data)

   */

}
