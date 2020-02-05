package arbo

import cats.Functor
import cats.instances.either._
import cats.instances.tuple._

import higherkindness.droste._

package object elgot {

  type ElgotCoalgebra[E[_], F[_], A] = GCoalgebra[Lambda[a => E[F[a]]], A, A]
  type ElgotAlgebra[E[_], F[_], A] = GAlgebra[Lambda[a => E[F[a]]], A, A]

  object ElgotCoalgebra extends scala.AnyRef {
    def apply[E[_], F[_], A](f: A => E[F[A]]): ElgotCoalgebra[E, F, A] =
      GCoalgebra[Lambda[a => E[F[a]]], A, A](f)
  }

  object ElgotAlgebra extends scala.AnyRef {
    def apply[E[_], F[_], A](f: E[F[A]] => A): ElgotAlgebra[E, F, A] = GAlgebra[Lambda[a => E[F[a]]], A, A](f)
  }

  def elgot[F[_] : Functor, A, B](
    algebra: Algebra[F, B],
    coalgebra: ElgotCoalgebra[Either[B, *], F, A]
  ): A => B =
    kernel.hyloC[Either[B, *], F, A, B](
      _.fold(identity[B], algebra.apply),
      coalgebra.run)

  def coelgot[F[_] : Functor, A, B](
    algebra: ElgotAlgebra[(A, *), F, B],
    coalgebra: Coalgebra[F, A]
  ): A => B =
    kernel.hyloC[(A, *), F, A, B](
      algebra.run,
      a => (a, coalgebra(a)))

  // Anamorphism that allows shortcuts
  def micro[F[_] : Functor, A, B](
    coalgebra: ElgotCoalgebra[Either[B, *], F, A]
  )(implicit embed: Embed[F, B]): A => B =
    elgot[F, A, B](embed.algebra, coalgebra)

}
