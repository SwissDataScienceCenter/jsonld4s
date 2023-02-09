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

package io.renku.jsonld

import cats.syntax.all._
import cats.data.NonEmptyList
import io.circe.{DecodingFailure, JsonNumber}
import io.renku.jsonld.Cursor._
import io.renku.jsonld.JsonLD._
import io.renku.jsonld.JsonLDDecoder.Result

import java.time.{Instant, LocalDate, OffsetDateTime}

/** A type class that provides a conversion from a [[Cursor]] to an object of type `A`
  */
trait JsonLDDecoder[A] extends (Cursor => Result[A]) with Serializable {

  def apply(cursor: Cursor): Result[A]

  def emap[B](f: A => Either[String, B]): JsonLDDecoder[B] =
    this(_).flatMap(f(_).leftMap(DecodingFailure(_, Nil)))
}

object JsonLDDecoder {

  type Result[A] = Either[DecodingFailure, A]

  final def apply[A](implicit jsonLDDecoder: JsonLDDecoder[A]): JsonLDDecoder[A] = jsonLDDecoder

  final def instance[A](f: Cursor => Result[A]): JsonLDDecoder[A] = (c: Cursor) => f(c)

  final def entity[A](
      entityTypes: EntityTypes,
      predicate:   Cursor => JsonLDDecoder.Result[Boolean] = _ => Right(true)
  )(f: Cursor => Result[A]): JsonLDEntityDecoder[A] =
    new JsonLDEntityDecoder[A](entityTypes, predicate, useCache = false)(f)

  final def cacheableEntity[A](
      entityTypes: EntityTypes,
      predicate:   Cursor => JsonLDDecoder.Result[Boolean] = _ => Right(true)
  )(f: Cursor => Result[A]): JsonLDEntityDecoder[A] =
    new JsonLDEntityDecoder[A](entityTypes, predicate, useCache = true)(f)

  implicit val decodeJsonLD: JsonLDDecoder[JsonLD] = _.jsonLD.asRight[DecodingFailure]

  implicit val decodeString: JsonLDDecoder[String] = _.jsonLD match {
    case JsonLDValue(value: String, _) => Right(value)
    case JsonLDValue(value, _)         => DecodingFailure(s"Cannot decode $value to String", Nil).asLeft
    case json                          => DecodingFailure(s"Cannot decode ${ShowTypeName(json)} to String", Nil).asLeft
  }

  implicit val decodeLong: JsonLDDecoder[Long] = _.jsonLD match {
    case JsonLDValue(value: JsonNumber, _) =>
      value.toLong.map(_.asRight).getOrElse(DecodingFailure(s"Cannot decode $value to Long", Nil).asLeft)
    case JsonLDValue(value, _) => DecodingFailure(s"Cannot decode $value to Long", Nil).asLeft
    case json                  => DecodingFailure(s"Cannot decode ${ShowTypeName(json)} to Long", Nil).asLeft
  }

  implicit val decodeInt: JsonLDDecoder[Int] = _.jsonLD match {
    case JsonLDValue(value: JsonNumber, _) =>
      value.toInt.map(_.asRight).getOrElse(DecodingFailure(s"Cannot decode $value to Int", Nil).asLeft)
    case JsonLDValue(value, _) => DecodingFailure(s"Cannot decode $value to Int", Nil).asLeft
    case json                  => DecodingFailure(s"Cannot decode ${ShowTypeName(json)} to Int", Nil).asLeft
  }

  implicit val decodeBoolean: JsonLDDecoder[Boolean] = _.jsonLD match {
    case JsonLDValue(value: Boolean, _) => Right(value)
    case JsonLDValue(value, _)          => DecodingFailure(s"Cannot decode $value to Boolean", Nil).asLeft
    case json => DecodingFailure(s"Cannot decode ${ShowTypeName(json)} to Boolean", Nil).asLeft
  }

  implicit val decodeInstant: JsonLDDecoder[Instant] = _.jsonLD match {
    case JsonLDValue(value: Instant, Some(JsonLDInstantValue.entityTypes)) => Right(value)
    case JsonLDValue(value: String, _) =>
      (Either.catchNonFatal(Instant.parse(value)) orElse Either.catchNonFatal(OffsetDateTime.parse(value).toInstant))
        .leftMap(e => DecodingFailure(s"Cannot decode $value to Instant: ${e.getMessage}", Nil))
    case JsonLDValue(value, _) => DecodingFailure(s"Cannot decode $value to Instant", Nil).asLeft
    case json                  => DecodingFailure(s"Cannot decode ${ShowTypeName(json)} to Instant", Nil).asLeft
  }

  implicit val decodeLocalDate: JsonLDDecoder[LocalDate] = _.jsonLD match {
    case JsonLDValue(value: LocalDate, Some(JsonLDLocalDateValue.entityTypes)) => Right(value)
    case JsonLDValue(value: String, _) =>
      Either
        .catchNonFatal(LocalDate.parse(value))
        .leftMap(e => DecodingFailure(s"Cannot decode $value to LocalDate: ${e.getMessage}", Nil))
    case JsonLDValue(value, _) => DecodingFailure(s"Cannot decode $value to LocalDate", Nil).asLeft
    case json                  => DecodingFailure(s"Cannot decode ${ShowTypeName(json)} to LocalDate", Nil).asLeft
  }

  implicit val decodeEntityId: JsonLDDecoder[EntityId] = _.jsonLD match {
    case JsonLDEntityId(id)        => Right(id)
    case JsonLDEntity(id, _, _, _) => Right(id)
    case json                      => DecodingFailure(s"Cannot decode ${ShowTypeName(json)} to EntityId", Nil).asLeft
  }

  implicit val decodeEntityTypes: JsonLDDecoder[EntityTypes] = _.jsonLD match {
    case JsonLDEntity(_, entityTypes, _, _) => Right(entityTypes)
    case json => DecodingFailure(s"Cannot decode ${ShowTypeName(json)} to EntityTypes", Nil).asLeft
  }

  implicit def decodeOption[I](implicit valueDecoder: JsonLDDecoder[I]): JsonLDDecoder[Option[I]] = { cursor =>
    cursor.jsonLD match {
      case JsonLD.JsonLDNull       => None.asRight[DecodingFailure]
      case JsonLD.JsonLDArray(Nil) => None.asRight[DecodingFailure]
      case JsonLD.JsonLDArray(firstItem :: Nil) =>
        cursor match {
          case cursor: FlattenedArrayCursor => valueDecoder(cursor downTo firstItem) map Option.apply
          case _ => valueDecoder(ListItemCursor.from(cursor, firstItem)) map Option.apply
        }
      case _ => valueDecoder(cursor) map Option.apply
    }
  }

  implicit def decodeList[I](implicit itemDecoder: JsonLDDecoder[I]): JsonLDDecoder[List[I]] = new JsonLDListDecoder[I]

  implicit def decodeSet[I](implicit itemDecoder: JsonLDDecoder[I]): JsonLDDecoder[Set[I]] =
    decodeList[I].emap(_.toSet.asRight)

  implicit def decodeNonEmptyList[I](implicit dt: JsonLDDecoder[I]): JsonLDDecoder[NonEmptyList[I]] =
    decodeList[I].emap(list => NonEmptyList.fromList(list).toRight(s"Expected a non-empty list"))
}

class JsonLDEntityDecoder[A](
    val entityTypes:                           EntityTypes,
    val predicate:                             Cursor => Result[Boolean],
    private[JsonLDEntityDecoder] val useCache: Boolean
)(f: Cursor => Result[A])
    extends JsonLDDecoder[A] { self =>

  implicit lazy val cacheableDecoder: CacheableEntityDecoder =
    if (useCache) CacheableEntityDecoder.Yes(this) else CacheableEntityDecoder.No

  override def emap[B](fn: A => Either[String, B]): JsonLDEntityDecoder[B] =
    new JsonLDEntityDecoder[B](entityTypes, predicate, useCache)(
      f.andThen(_.flatMap(v => fn(v).left.map(DecodingFailure(_, Nil))))
    )

  override def apply(cursor: Cursor): Result[A] = cursor match {
    case cur: FlattenedJsonCursor => tryDecode(cur) getOrElse cannotDecodeToEntityTypes(cur)
    case _ => goDownType(cursor)
  }

  private lazy val cannotDecodeToEntityTypes: Cursor => Result[A] = _.jsonLD match {
    case entity: JsonLDEntity =>
      DecodingFailure(
        show"Cannot decode entity with ${entity.id} to entity of type(s) $entityTypes; check entity decoder predicate",
        Nil
      ).asLeft
    case _ => DecodingFailure(show"Cannot find an entity of type(s) $entityTypes", Nil).asLeft
  }

  lazy val allowedEntityTypes: Set[EntityTypes] = Set(entityTypes)

  def widen[B >: A]: JsonLDEntityDecoder[B] = this.asInstanceOf[JsonLDEntityDecoder[B]]

  def orElse[B >: A](alternative: JsonLDEntityDecoder[B]): JsonLDEntityDecoder[B] =
    new JsonLDEntityDecoder[B](entityTypes, predicate, alternative.useCache)(f) {

      override def apply(cursor: Cursor): Result[B] = cursor match {
        case cur: FlattenedJsonCursor =>
          self.tryDecode(cur) orElse alternative.tryDecode(cur) getOrElse noEntityWithTypesFailure
        case _ => goDownType(cursor)
      }

      private lazy val noEntityWithTypesFailure = DecodingFailure(
        show"Cannot find neither an entity of type(s) ${self.entityTypes} nor ${alternative.entityTypes}",
        Nil
      ).asLeft

      override lazy val allowedEntityTypes: Set[EntityTypes] =
        self.allowedEntityTypes ++ alternative.allowedEntityTypes

      protected override def goDownType(cursor: Cursor) =
        self.goDownType(cursor) orElse alternative.goDownType(cursor)
    }

  protected def tryDecode(cursor: FlattenedJsonCursor): Option[Result[A]] =
    cursor.findInCache[A] match {
      case Some(fromCache) => fromCache.asRight.some
      case _ =>
        cursor
          .findEntity(entityTypes, predicate)
          .map { case entityJson @ JsonLDEntity(id, _, _, _) =>
            val cursorDown = cursor.downTo(entityJson)
            goDownType(cursorDown).flatTap(obj => cursorDown.cache(id, obj).asRight)
          }
    }

  protected def goDownType(cursor: Cursor): Result[A] = cursor.downType(entityTypes) match {
    case cursor @ Cursor.Empty(_) =>
      DecodingFailure(show"Cannot decode to an entity of type(s) $entityTypes $cursor", Nil).asLeft
    case cursor => f(cursor)
  }
}

private[jsonld] sealed trait CacheableEntityDecoder extends Product with Serializable

private[jsonld] object CacheableEntityDecoder {

  def yes: JsonLDEntityDecoder[_] => CacheableEntityDecoder.Yes     = Yes
  def no:  JsonLDEntityDecoder[_] => CacheableEntityDecoder.No.type = _ => No

  final case class Yes(decoder: JsonLDEntityDecoder[_]) extends CacheableEntityDecoder
  final case object No                                  extends CacheableEntityDecoder
}

private[jsonld] class JsonLDListDecoder[I](implicit itemDecoder: JsonLDDecoder[I]) extends JsonLDDecoder[List[I]] {
  import JsonLDDecoder._

  override def apply(cursor: Cursor): Result[List[I]] = cursor match {
    case cursor: FlattenedArrayCursor => decodeIfFlattenedArrayCursor(cursor)
    case cursor: ArrayCursor          => decodeIfArrayCursor(cursor)
    case cursor: FlattenedJsonCursor  => decodeIfFlattenedCursor(cursor)
    case cursor => decodeIfOtherCursor(cursor)
  }

  private lazy val decodeIfFlattenedArrayCursor: FlattenedArrayCursor => Result[List[I]] = {
    case cursor @ FlattenedArrayCursor(_, array, _) =>
      itemDecoder match {
        case itemDecoder: JsonLDEntityDecoder[I] =>
          for {
            ids            <- array.cursor.as[List[EntityId]]
            entityOrCached <- (ids map fromCacheOrFromAllEntities(cursor)).sequence
            filtered       <- entityOrCached.map(entitiesFor(itemDecoder)).collect(matchingEntities).asRight
            decoded        <- filtered.map(decode(cursor.downTo)).sequence
          } yield decoded
        case itemDecoder: JsonLDDecoder[I] =>
          cursor.jsonLD.jsons.toList.map { json =>
            itemDecoder(ListItemCursor.from(cursor, json)).flatTap(cursor.cache(json, _, itemDecoder).asRight)
          }.sequence
      }
  }

  private def fromCacheOrFromAllEntities(cursor: FlattenedArrayCursor)(id: EntityId) =
    cursor.findInCache(id, itemDecoder) match {
      case Some(fromCache) => fromCache.asRight[JsonLDEntity].asRight[DecodingFailure]
      case None =>
        Either
          .fromOption(cursor.allEntities.get(id), DecodingFailure(s"No entity with id $id", Nil))
          .map(_.asLeft[I])
    }

  private def entitiesFor(
      decoder: JsonLDEntityDecoder[I]
  ): Either[JsonLDEntity, I] => (Boolean, Either[JsonLDEntity, I]) = {
    case entityOrCached @ Right(_) => true -> entityOrCached
    case entityOrCached @ Left(entity) =>
      val typesAndPredicateOK =
        decoder.allowedEntityTypes.exists(allowed => entity.entityTypes.exists(_ contains allowed)) &&
          decoder.predicate(entity.cursor).fold(_ => false, identity)
      typesAndPredicateOK -> entityOrCached
  }

  private def matchingEntities: PartialFunction[(Boolean, Either[JsonLDEntity, I]), Either[JsonLDEntity, I]] = {
    case (true, entityOrCached) => entityOrCached
  }

  private def decode(cursorFactory: JsonLDEntity => Cursor): Either[JsonLDEntity, I] => Result[I] = {
    case Right(cached) => cached.asRight[DecodingFailure]
    case Left(entity @ JsonLDEntity(id, _, _, _)) =>
      val cursor = cursorFactory(entity)
      itemDecoder(cursor)
        .leftMap(failure => DecodingFailure(show"Cannot decode entity with $id: $failure", Nil))
        .flatTap(cursor.cache(entity, _, itemDecoder).asRight)
  }

  private def decodeIfFlattenedCursor(cursor: FlattenedJsonCursor): Result[List[I]] = cursor.jsonLD match {
    case entity @ JsonLDEntity(entityId, _, _, _) =>
      cursor.findInCache(itemDecoder) match {
        case Some(fromCache) => fromCache.asRight.map(List(_))
        case None => cursor.downTo(entity).as[I].flatTap(cursor.cache(entityId, _, itemDecoder).asRight).map(List(_))
      }
    case JsonLDEntityId(entityId) =>
      cursor.findInCache(itemDecoder) match {
        case Some(fromCache) => fromCache.asRight.map(List(_))
        case _ =>
          cursor
            .findEntityById(entityId)
            .map(cursor.downTo(_).as[I].flatTap(cursor.cache(entityId, _, itemDecoder).asRight).map(List(_)))
            .getOrElse(DecodingFailure(show"Cannot find entity with $entityId", Nil).asLeft)
      }
    case JsonLDArray(jsons) => jsons.toList.map(cursor.downTo).map(_.as[I]).sequence
    case _                  => cursor.as[I].map(List(_))
  }

  private def decodeIfArrayCursor: ArrayCursor => Result[List[I]] = {
    case cursor @ ArrayCursor(_, JsonLDArray(jsons)) =>
      itemDecoder match {
        case decoder: JsonLDEntityDecoder[I] =>
          decodeEntities(cursor, _ => ListItemCursor.from(cursor, _), jsons, decoder)
        case decoder =>
          jsons.toList
            .map(json => decoder(ListItemCursor.from(cursor, json)).flatTap(cursor.cache(json, _, decoder).asRight))
            .sequence
      }
  }

  private def decodeEntities(cursor:        Cursor,
                             cursorFactory: Map[EntityId, JsonLDEntity] => JsonLDEntity => Cursor,
                             jsons:         Seq[JsonLD],
                             decoder:       JsonLDEntityDecoder[I]
  ) = for {
    allEntitiesMap <- jsons.toList.map(_.cursor.as[(EntityId, JsonLDEntity)]).sequence
    entityOrCached <- (allEntitiesMap map tryCache(cursor)).sequence
    filtered       <- entityOrCached.map(entitiesFor(decoder)).collect(matchingEntities).asRight
    decoded        <- filtered.map(decode(cursorFactory(allEntitiesMap.toMap))).sequence
  } yield decoded

  private def tryCache(cursor: Cursor): ((EntityId, JsonLDEntity)) => Result[Either[JsonLDEntity, I]] = {
    case (id, entity) =>
      cursor.findInCache(id, itemDecoder) match {
        case Some(fromCache) => fromCache.asRight[JsonLDEntity].asRight[DecodingFailure]
        case None            => entity.asLeft[I].asRight[DecodingFailure]
      }
  }

  private def decodeIfOtherCursor(cursor: Cursor): Result[List[I]] = cursor.jsonLD match {
    case JsonLDArray(jsons) =>
      itemDecoder match {
        case decoder: JsonLDEntityDecoder[I] =>
          decodeEntities(cursor,
                         allEntities => entity => FlattenedJsonCursor.from(cursor, entity, allEntities),
                         jsons,
                         decoder
          )
        case decoder: JsonLDDecoder[I] =>
          jsons.toList
            .map(json => decoder(ListItemCursor.from(cursor, json)).flatTap(cursor.cache(json, _, decoder).asRight))
            .sequence
      }
    case entity @ JsonLDEntity(_, _, _, _) =>
      entity.cursor
        .as[I]
        .flatTap(cursor.cache(entity, _, itemDecoder).asRight)
        .map(List(_))
    case value @ JsonLDValue(_, _) => value.cursor.as[I].map(List(_))
    case JsonLDNull                => List.empty[I].asRight
    case jsonLD                    => DecodingFailure(s"Cannot decode ${ShowTypeName(jsonLD)} to List", Nil).asLeft
  }

  private implicit lazy val decodeJsonLDEntity: JsonLDDecoder[(EntityId, JsonLDEntity)] = _.jsonLD match {
    case entity @ JsonLDEntity(id, _, _, _) => Right(id -> entity)
    case json => DecodingFailure(s"Cannot decode ${ShowTypeName(json)} to JsonLDEntity", Nil).asLeft
  }
}

private object ShowTypeName {
  def apply(json: JsonLD) = json.getClass.getSimpleName.replace("$", "")
}
