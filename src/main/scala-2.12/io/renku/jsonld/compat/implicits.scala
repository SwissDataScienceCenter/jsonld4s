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

package io.renku.jsonld.compat

import scala.collection.{IterableView, immutable, mutable}
import cats.{Alternative, Eval, Foldable, Traverse}

import scala.language.higherKinds

object implicits {

  implicit def catsImplicitForSeq: Foldable[Seq] with Alternative[Seq] =
    new Foldable[Seq] with Alternative[Seq] {
      override def foldLeft[A, B](fa: Seq[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
      override def foldRight[A, B](fa: Seq[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa, lb)(f)
      override def pure[A](x: A): Seq[A] = Seq(x)
      override def empty[A]: Seq[A] = Seq.empty
      override def combineK[A](x: Seq[A], y: Seq[A]): Seq[A] = x ++ y
      override def ap[A, B](ff: Seq[A => B])(fa: Seq[A]): Seq[B] = ff.flatMap(f => fa.map(f))
    }

  implicit class IterableOpsCompat[A](val iterable: Iterable[A]) extends AnyVal {
    def groupMapReduce[K, B](key: A => K)(f: A => B)(reduce: (B, B) => B): immutable.Map[K, B] = {
      val m = mutable.Map.empty[K, B]
      for (elem <- iterable) {
        val k = key(elem)
        val v =
          m.get(k) match {
            case Some(b) => reduce(b, f(elem))
            case None => f(elem)
          }
        m.put(k, v)
      }
      m.toMap
    }
  }

  implicit class TraverseOpsEitherCompat[F[_], A, B](val value: F[Either[A, B]]) extends AnyVal {
    def sequence(implicit F: Traverse[F]): Either[A, F[B]] = F.sequence[Either[A, *], B](value)
  }

  implicit class FlatMapOpsEitherCompat[A, B](val value: Either[A, B]) extends AnyVal {
    def >>=[C](f: B => Either[A, C]): Either[A, C] = value match {
      case Left(a)  => Left(a)
      case Right(b) => f(b)
    }
  }

  implicit class IterableViewOpsCompat[K, V](val value: IterableView[(K, V), Map[K, V]]) extends AnyVal {
    def filterKeys(p: K => Boolean): Map[K, V] = value.filter { case (k, _) => p(k) }.toMap
  }

}
