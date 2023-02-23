package io.renku.jsonld.compat

import scala.collection.mutable
import scala.collection.immutable
import cats.{Alternative, Eval, Foldable}

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

}
