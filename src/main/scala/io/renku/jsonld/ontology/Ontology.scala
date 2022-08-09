/*
 * Copyright 2022 Swiss Data Science Center (SDSC)
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

import cats.kernel.Semigroup
import cats.syntax.all._
import io.renku.jsonld.JsonLDEncoder._
import io.renku.jsonld._
import io.renku.jsonld.ontology.DataProperty.TopDataProperty
import io.renku.jsonld.ontology.ObjectProperty.TopObjectProperty
import io.renku.jsonld.syntax._

final case class Type(clazz: Class, objectProperties: List[ObjectProperty], dataProperties: List[DataProperty]) {

  lazy val flatten: (List[Class], List[ObjectProperty], List[DataProperty]) =
    objectProperties.flatMap(_.range).map(_.typ.flatten).foldLeft((List(clazz), objectProperties, dataProperties)) {
      case ((allClasses, allObjProps, allDataProps), (classes, objProps, dataProps)) =>
        (allClasses |+| classes |+| objProps.flatMap(_.range).map(_.typ.clazz),
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

  def Def(clazz: Class, dataProperty1: DataProperty.Def, otherDataProperties: DataProperty.Def*): Type =
    Def(clazz, ObjectProperties.empty, dataProperty1 :: otherDataProperties.toList)

  def Def(clazz: Class, objProperty1: ObjectProperty.Def, otherObjProperties: ObjectProperty.Def*): Type =
    Def(clazz, objProperty1 :: otherObjProperties.toList, DataProperties.empty)

  def Def(clazz: Class, objectProperties: List[ObjectProperty.Def], dataProperties: List[DataProperty.Def]): Type =
    Type(clazz, objectProperties.map(_.of(clazz)), dataProperties.map(_.of(clazz)))

  implicit def encoder: JsonLDEncoder[Type] = JsonLDEncoder.instance {
    _.flatten match {
      case (classes, objectProperties, dataProperties) =>
        JsonLD.arr(
          classes.map(_.asJsonLD) ::: objectProperties.map(_.asJsonLD) ::: dataProperties.map(_.asJsonLD): _*
        )
    }
  }
}

final case class Class(id: EntityId)
object Class {

  def apply(clazz: EntityId): Class = new Class(EntityId.of(clazz))

  implicit lazy val encoder: JsonLDEncoder[Class] = JsonLDEncoder.instance { case Class(id) =>
    JsonLD.entity(
      id,
      EntityTypes of owl / "Class",
      Map.empty[Property, JsonLD]
    )
  }
}

final case class ObjectProperty(id:          EntityId,
                                range:       List[ObjectPropertyRange],
                                domain:      List[Domain],
                                subProperty: Option[TopObjectProperty]
)

object ObjectProperty {

  final case class Def(id: EntityId, range: List[ObjectPropertyRange], subProperty: Option[TopObjectProperty]) {
    def of(clazz: Class): ObjectProperty =
      ObjectProperty(id, range, domain = List(Domain(clazz)), subProperty)
  }

  case object TopObjectProperty {
    implicit lazy val encoder: JsonLDEncoder[TopObjectProperty] = JsonLDEncoder.instance { _ =>
      JsonLD.fromEntityId(owl / "topObjectProperty")
    }
  }
  type TopObjectProperty = TopObjectProperty.type

  def apply(id: EntityId, range: Type): Def = Def(id, List(ObjectPropertyRange(range)), subProperty = None)

  def top(id: EntityId, range: Type): Def =
    Def(id, List(ObjectPropertyRange(range)), subProperty = Some(TopObjectProperty))

  implicit lazy val encoder: JsonLDEncoder[ObjectProperty] = JsonLDEncoder.instance {
    case ObjectProperty(id, range, domain, maybeSubProperty) =>
      JsonLD.entity(
        id,
        EntityTypes of owl / "ObjectProperty",
        rdfs / "domain"        -> domain.asJsonLD,
        rdfs / "range"         -> range.asJsonLD,
        rdfs / "subPropertyOf" -> maybeSubProperty.asJsonLD
      )
  }
}

object ObjectProperties {
  lazy val empty:                             List[ObjectProperty.Def] = Nil
  def apply(properties: ObjectProperty.Def*): List[ObjectProperty.Def] = properties.toList
}

final case class ObjectPropertyRange(typ: Type)
object ObjectPropertyRange {

  implicit lazy val encoder: JsonLDEncoder[ObjectPropertyRange] = JsonLDEncoder.instance { range =>
    JsonLD.fromEntityId(range.typ.clazz.id)
  }
}

final case class DataProperty(id:          EntityId,
                              range:       List[DataPropertyRange],
                              domain:      List[Domain],
                              subProperty: Option[TopDataProperty]
)
object DataProperty {

  case object TopDataProperty {
    implicit lazy val encoder: JsonLDEncoder[TopDataProperty] = JsonLDEncoder.instance { _ =>
      JsonLD.fromEntityId(owl / "topDataProperty")
    }
  }

  type TopDataProperty = TopDataProperty.type

  final case class Def(id: EntityId, range: List[DataPropertyRange], subProperty: Option[TopDataProperty]) {
    def of(clazz: Class): DataProperty = DataProperty(id, range, domain = List(Domain(clazz)), subProperty)
  }

  def apply(id: EntityId, range1: Property, otherRanges: Property*): DataProperty.Def =
    DataProperty.Def(id, (range1 :: otherRanges.toList).map(DataPropertyRange(_)), subProperty = None)

  def top(id: EntityId, range1: Property, otherRanges: Property*): DataProperty.Def =
    DataProperty.Def(id, (range1 :: otherRanges.toList).map(DataPropertyRange(_)), Some(TopDataProperty))

  implicit lazy val encoder: JsonLDEncoder[DataProperty] = JsonLDEncoder.instance {
    case DataProperty(id, range, domain, maybeSubProperty) =>
      JsonLD
        .entity(
          id,
          EntityTypes of owl / "DatatypeProperty",
          rdfs / "domain"        -> domain.asJsonLD,
          rdfs / "range"         -> range.asJsonLD,
          rdfs / "subPropertyOf" -> maybeSubProperty.asJsonLD
        )
  }
}

object DataProperties {
  lazy val empty:                           List[DataProperty.Def] = Nil
  def apply(properties: DataProperty.Def*): List[DataProperty.Def] = properties.toList
}

final case class Domain(clazz: Class)
object Domain {
  implicit lazy val encoder: JsonLDEncoder[Domain] = JsonLDEncoder.instance { case Domain(objClass) =>
    JsonLD.fromEntityId(objClass.id)
  }
}

final case class DataPropertyRange(id: Property)
object DataPropertyRange {
  implicit lazy val encoder: JsonLDEncoder[DataPropertyRange] = JsonLDEncoder.instance { case DataPropertyRange(id) =>
    JsonLD.fromEntityId(id)
  }
}
