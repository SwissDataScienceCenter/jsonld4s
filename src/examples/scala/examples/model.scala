package examples

final case class User(name: String)
final case class Project(name: String, members: List[User])
final case class Organization(name: String, legalName: Option[String])
