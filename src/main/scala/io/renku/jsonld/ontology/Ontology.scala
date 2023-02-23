/*
 * Copyright 2023 Swiss Data Science Center (SDSC)
 * A partnership between École Polytechnique Fédérale de Lausanne (EPFL) and
 * Eidgenössische Technische Hochschule Zürich (ETHZ).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.renku.jsonld.ontology

import cats.Show
import cats.data.NonEmptyList
import cats.evidence.{<~<, ===}
import cats.kernel.Semigroup
import cats.syntax.all._
import io.renku.jsonld.JsonLDEncoder._
import io.renku.jsonld._
import io.renku.jsonld.ontology.DataProperty.TopDataProperty
import io.renku.jsonld.ontology.ObjectProperty.TopObjectProperty
import io.renku.jsonld.syntax._
import io.renku.jsonld.compat.implicits._

final case class Type(clazz:             Class,
                      objectProperties:  List[ObjectProperty],
                      dataProperties:    List[DataProperty],
                      reverseProperties: List[ReverseProperty]
) {

  type FlatteningResults = (List[Class], List[ObjectProperty], List[DataProperty])

  lazy val flatten: FlatteningResults =
    reverseProperties.map(_.factory(clazz)).map(_.flatten).foldLeft(directPropsResults) {
      case ((allClasses, allObjProps, allDataProps), (reverseClasses, reverseObjProps, reverseDataProps)) =>
        (allClasses |+| reverseClasses, allObjProps |+| reverseObjProps, allDataProps |+| reverseDataProps)
    }

  private lazy val directPropsResults: FlatteningResults =
    objectProperties.flatMap(_.range).foldLeft((List(clazz), objectProperties, dataProperties)) {
      case (allCollected @ (allClasses, _, _), ObjectPropertyRange.OfType(typ)) if allClasses contains typ.clazz =>
        allCollected
      case ((allClasses, allObjProps, allDataProps), ObjectPropertyRange.OfClass(clazz)) =>
        (allClasses |+| List(clazz), allObjProps, allDataProps)
      case ((allClasses, allObjProps, allDataProps), objPropRange: ObjectPropertyRange.OfType) =>
        val (classes, objProps, dataProps) = objPropRange.typ.flatten
        (allClasses |+| classes |+| objProps.flatMap(_.range).map(_.clazz),
         allObjProps |+| objProps,
         allDataProps |+| dataProps
        )
    }

  private implicit lazy val classesSemigroup: Semigroup[List[Class]] =
    (x: List[Class], y: List[Class]) => (x ::: y).distinct

  private implicit lazy val objPropertiesSemigroup: Semigroup[List[ObjectProperty]] =
    (x: List[ObjectProperty], y: List[ObjectProperty]) =>
      (x ::: y).distinct
        .groupMapReduce(_.id)(identity) { case (left, right) =>
          left.copy(
            range = (left.range ::: right.range).distinct,
            domain = (left.domain ::: right.domain).distinct
          )
        }
        .values
        .toList

  private implicit lazy val dataPropertiesSemigroup: Semigroup[List[DataProperty]] =
    (x: List[DataProperty], y: List[DataProperty]) =>
      (x ::: y).distinct
        .groupMapReduce(_.id)(identity) { case (left, right) =>
          left.copy(
            range = (left.range ::: right.range).distinct,
            domain = (left.domain ::: right.domain).distinct
          )
        }
        .values
        .toList
}

object Type {

  def Def(clazz: Class): Type = Def(clazz, ObjectProperties.empty, DataProperties.empty, ReverseProperties.empty)

  def Def(clazz: Class, dataProperty1: DataProperty.Def, otherDataProperties: DataProperty.Def*): Type =
    Def(clazz, ObjectProperties.empty, dataProperty1 :: otherDataProperties.toList, ReverseProperties.empty)

  def Def(clazz: Class, objProperty1: ObjectProperty.Def, otherObjProperties: ObjectProperty.Def*): Type =
    Def(clazz, objProperty1 :: otherObjProperties.toList, DataProperties.empty, ReverseProperties.empty)

  def Def(clazz:             Class,
          objectProperties:  List[ObjectProperty.Def],
          dataProperties:    List[DataProperty.Def],
          reverseProperties: List[ReverseProperty] = ReverseProperties.empty
  ): Type = Type(clazz, objectProperties.map(_.of(clazz)), dataProperties.map(_.of(clazz)), reverseProperties)

  implicit def encoder: JsonLDEncoder[Type] = JsonLDEncoder.instance {
    _.flatten match {
      case (classes, objectProperties, dataProperties) =>
        JsonLD.arr(
          classes.map(_.asJsonLD) ::: objectProperties.map(_.asJsonLD) ::: dataProperties.map(_.asJsonLD): _*
        )
    }
  }
}

final case class Class(id: EntityId, parentClasses: List[ParentClass], maybeComment: Option[Comment]) {
  def withComment(comment: String): Class = copy(maybeComment = Some(Comment(comment)))
}
object Class {

  def apply(clazz: EntityId, maybeComment: Option[Comment] = None): Class =
    new Class(EntityId.of(clazz), parentClasses = Nil, maybeComment)
  def apply(clazz: EntityId, parentClass: ParentClass, otherParentClasses: ParentClass*): Class =
    new Class(EntityId.of(clazz), parentClasses = parentClass :: otherParentClasses.toList, maybeComment = None)

  implicit lazy val encoder: JsonLDEncoder[Class] = JsonLDEncoder.instance {
    case Class(id, parentClasses, maybeComment) =>
      JsonLD.entity(
        id,
        EntityTypes of owl / "Class",
        Seq(
          parentClasses match {
            case Nil => Option.empty[(Property, JsonLD)]
            case _   => Some(rdfs / "subClassOf" -> parentClasses.asJsonLD)
          },
          maybeComment.map(c => rdfs / "comment" -> c.asJsonLD)
        ).flatten.toMap
      )
  }
}

final case class ParentClass(id: EntityId)
object ParentClass {

  def apply(typ: Type): ParentClass = ParentClass(typ.clazz.id)

  implicit lazy val encoder: JsonLDEncoder[ParentClass] = JsonLDEncoder.instance { case ParentClass(id) =>
    id.asJsonLD
  }
}

final case class ObjectProperty(id:               EntityId,
                                range:            List[ObjectPropertyRange],
                                domain:           List[Domain],
                                maybeSubProperty: Option[TopObjectProperty],
                                maybeComment:     Option[Comment]
) {
  def withComment(comment: String): ObjectProperty = copy(maybeComment = Some(Comment(comment)))
}

object ObjectProperty {

  final case class Def(id:               EntityId,
                       range:            List[ObjectPropertyRange],
                       maybeSubProperty: Option[TopObjectProperty],
                       maybeComment:     Option[Comment]
  ) {

    def withComment(comment: String): Def = copy(maybeComment = Some(Comment(comment)))

    def of(clazz: Class): ObjectProperty =
      ObjectProperty(id, range, domain = List(Domain(clazz)), maybeSubProperty, maybeComment)
  }

  case object TopObjectProperty {
    implicit lazy val encoder: JsonLDEncoder[TopObjectProperty] = JsonLDEncoder.instance { _ =>
      JsonLD.fromEntityId(owl / "topObjectProperty")
    }
  }
  type TopObjectProperty = TopObjectProperty.type

  def apply(id: EntityId, range: Type, maybeComment: Option[Comment] = None): Def =
    Def(id, List(ObjectPropertyRange(range)), maybeSubProperty = None, maybeComment)

  def apply(id: EntityId, range: ObjectPropertyRange, otherRanges: ObjectPropertyRange*): Def =
    Def(id, range :: otherRanges.toList, maybeSubProperty = None, maybeComment = None)

  def apply(id: EntityId, range: Class, otherRanges: Class*): Def =
    Def(id, (range :: otherRanges.toList).map(ObjectPropertyRange(_)), maybeSubProperty = None, maybeComment = None)

  def apply(id: EntityId, range: Type, otherRanges: Type*): Def =
    Def(id, (range :: otherRanges.toList).map(ObjectPropertyRange(_)), maybeSubProperty = None, maybeComment = None)

  def top(id: EntityId, range: Type, maybeComment: Option[Comment] = None): Def =
    Def(id, List(ObjectPropertyRange(range)), maybeSubProperty = Some(TopObjectProperty), maybeComment)

  def top(id: EntityId, range: Type, otherRanges: Type*): Def =
    Def(id,
        (range :: otherRanges.toList).map(ObjectPropertyRange(_)),
        maybeSubProperty = Some(TopObjectProperty),
        maybeComment = None
    )

  implicit lazy val encoder: JsonLDEncoder[ObjectProperty] = JsonLDEncoder.instance {
    case ObjectProperty(id, range, domain, maybeTopProperty, maybeComment) =>
        JsonLD.entity(
          id,
          EntityTypes of owl / "ObjectProperty",
          Seq(
            Some(rdfs / "domain" -> domain.asJsonLD),
            Some(rdfs / "range"  -> range.asJsonLD),
            maybeTopProperty.map(p => rdfs / "subPropertyOf" -> p.encoder(p)),
            maybeComment.map(c => rdfs / "comment" -> c.asJsonLD)
          ).flatten.toMap
        )
  }
}

object ObjectProperties {
  lazy val empty: List[ObjectProperty.Def] = Nil
  def apply(properties: ObjectProperty.Def*): List[ObjectProperty.Def] = properties.toList
}

sealed trait ObjectPropertyRange {
  def clazz: Class
}
object ObjectPropertyRange {

  final case class OfType(typ: Type) extends ObjectPropertyRange {
    override lazy val clazz: Class = typ.clazz
  }
  final case class OfClass(clazz: Class) extends ObjectPropertyRange

  def apply(typ:   Type):  ObjectPropertyRange = OfType(typ)
  def apply(clazz: Class): ObjectPropertyRange = OfClass(clazz)

  implicit lazy val encoder: JsonLDEncoder[ObjectPropertyRange] = JsonLDEncoder.instance {
    case OfType(typ)    => JsonLD.fromEntityId(typ.clazz.id)
    case OfClass(clazz) => JsonLD.fromEntityId(clazz.id)
  }
}

final case class DataProperty(id:               Property,
                              range:            List[DataPropertyRange],
                              domain:           List[Domain],
                              maybeSubProperty: Option[TopDataProperty],
                              maybeComment:     Option[Comment]
) {
  def withComment(comment: String): DataProperty = copy(maybeComment = Some(Comment(comment)))
}
object DataProperty {

  case object TopDataProperty {
    implicit lazy val encoder: JsonLDEncoder[TopDataProperty] = JsonLDEncoder.instance { _ =>
      JsonLD.fromEntityId(owl / "topDataProperty")
    }
  }

  type TopDataProperty = TopDataProperty.type

  final case class Def(id:               Property,
                       range:            List[DataPropertyRange],
                       maybeSubProperty: Option[TopDataProperty],
                       maybeComment:     Option[Comment]
  ) {

    def withComment(comment: String): Def = copy(maybeComment = Some(Comment(comment)))

    def of(clazz: Class): DataProperty =
      DataProperty(id, range, domain = List(Domain(clazz)), maybeSubProperty, maybeComment)
  }

  def apply(id: Property, range1: Property, otherRanges: Property*): DataProperty.Def =
    DataProperty.Def(id,
                     (range1 :: otherRanges.toList).map(DataPropertyRange.Simple),
                     maybeSubProperty = None,
                     maybeComment = None
    )

  def apply(id: Property, range1: DataPropertyRange, otherRanges: DataPropertyRange*): DataProperty.Def =
    DataProperty.Def(id, range1 :: otherRanges.toList, maybeSubProperty = None, maybeComment = None)

  def top(id: Property, range1: Property, otherRanges: Property*): DataProperty.Def =
    DataProperty.Def(id,
                     (range1 :: otherRanges.toList).map(DataPropertyRange.Simple),
                     Some(TopDataProperty),
                     maybeComment = None
    )

  def top(id: Property, range1: DataPropertyRange, otherRanges: DataPropertyRange*): DataProperty.Def =
    DataProperty.Def(id, range1 :: otherRanges.toList, Some(TopDataProperty), maybeComment = None)

  implicit lazy val encoder: JsonLDEncoder[DataProperty] = JsonLDEncoder.instance {
    case DataProperty(id, range, domain, maybeTopProperty, maybeComment) =>
      JsonLD
        .entity(
          EntityId.of(id),
          EntityTypes of owl / "DatatypeProperty",
          Seq(
            Some(rdfs / "domain" -> domain.asJsonLD),
            Some(rdfs / "range"  -> range.asJsonLD),
            maybeTopProperty.map(p => rdfs / "subPropertyOf" -> p.encoder(p)),
            maybeComment.map(c => rdfs / "comment" -> c.asJsonLD)
          ).flatten.toMap
        )
  }
}

object DataProperties {
  lazy val empty: List[DataProperty.Def] = Nil
  def apply(properties: DataProperty.Def*): List[DataProperty.Def] = properties.toList
}

final case class Domain(clazz: Class)
object Domain {
  implicit lazy val encoder: JsonLDEncoder[Domain] = JsonLDEncoder.instance { case Domain(objClass) =>
    JsonLD.fromEntityId(objClass.id)
  }
}

sealed trait DataPropertyRange
object DataPropertyRange {

  final case class Simple(id: Property)                     extends DataPropertyRange
  final case class Enumeration(items: NonEmptyList[String]) extends DataPropertyRange

  def apply(id:   Property): DataPropertyRange = Simple(id)
  def apply(item: String, other: String*): DataPropertyRange = Enumeration(NonEmptyList.of(item, other: _*))
  def apply[T](items: NonEmptyList[T])(implicit show: Show[T]): DataPropertyRange = Enumeration(items.map(_.show))

  implicit lazy val encoder: JsonLDEncoder[DataPropertyRange] = JsonLDEncoder.instance[DataPropertyRange] {
    case Simple(id) => JsonLD.fromEntityId(id)
    case Enumeration(items) =>
      def toJson(headItem: String, tailItems: List[String]): JsonLD = tailItems match {
        case Nil =>
          JsonLD.entity(EntityId.blank,
                        EntityTypes of rdf / "List",
                        rdf / "first" -> headItem.asJsonLD,
                        rdf / "rest"  -> EntityId.of(rdf / "nil").asJsonLD
          )
        case newHead :: newTail =>
          JsonLD.entity(EntityId.blank,
                        EntityTypes of rdf / "List",
                        rdf / "first" -> headItem.asJsonLD,
                        rdf / "rest"  -> toJson(newHead, newTail)
          )
      }

      JsonLD.entity(
        EntityId.blank,
        EntityTypes of rdfs / "Datatype",
        owl / "oneOf" -> toJson(items.head, items.tail)
      )
  }
}

final case class Comment(value: String)
object Comment {
  implicit lazy val encoder: JsonLDEncoder[Comment] = JsonLDEncoder.encodeString.contramap(_.value)
}

final case class ReverseProperty(factory: Class => Type)
object ReverseProperties {
  lazy val empty: List[ReverseProperty] = Nil
  def apply(reverse: ReverseProperty, otherReverses: ReverseProperty*): List[ReverseProperty] =
    reverse :: otherReverses.toList
}
