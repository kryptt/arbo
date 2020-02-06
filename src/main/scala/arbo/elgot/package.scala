package arbo

import cats.{Functor, Monad, Traverse}
import cats.instances.either._
import cats.instances.tuple._
import cats.syntax.applicative._

import higherkindness.droste._

package object elgot {

  type ElgotCoalgebra[E[_], F[_], A] = GCoalgebra[Lambda[a => E[F[a]]], A, A]
  type ElgotAlgebra[E[_], F[_], A] = GAlgebra[Lambda[a => E[F[a]]], A, A]

  type ElgotCoalgebraM[M[_], E[_], F[_], A] = GCoalgebraM[M, Lambda[a => E[F[a]]], A, A]
  type ElgotAlgebraM[M[_], E[_], F[_], A] = GAlgebraM[M, Lambda[a => E[F[a]]], A, A]

  object ElgotCoalgebra extends scala.AnyRef {
    def apply[E[_], F[_], A](f: A => E[F[A]]): ElgotCoalgebra[E, F, A] =
      GCoalgebra[Lambda[a => E[F[a]]], A, A](f)
  }

  object ElgotCoalgebraM extends scala.AnyRef {
    def apply[M[_], E[_], F[_], A](f: A => M[E[F[A]]]): ElgotCoalgebraM[M, E, F, A] =
      GCoalgebraM[M, Lambda[a => E[F[a]]], A, A](f)
  }

  object ElgotAlgebra extends scala.AnyRef {
    def apply[E[_], F[_], A](f: E[F[A]] => A): ElgotAlgebra[E, F, A] = GAlgebra[Lambda[a => E[F[a]]], A, A](f)
  }

  object ElgotAlgebraM extends scala.AnyRef {
    def apply[M[_], E[_], F[_], A](f: E[F[A]] => M[A]): ElgotAlgebraM[M, E, F, A] = GAlgebraM[M, Lambda[a => E[F[a]]], A, A](f)
  }

  def elgot[F[_] : Functor, A, B](
    algebra: Algebra[F, B],
    coalgebra: ElgotCoalgebra[Either[B, *], F, A]
  ): A => B =
    kernel.hyloC[Either[B, *], F, A, B](
      _.fold(identity[B], algebra.apply),
      coalgebra.run)

  def elgotM[M[_]: Monad, F[_]: Traverse, A, B](
  algebra: Algebra[F, B],
  coalgebra: ElgotCoalgebraM[M, Either[B, *], F, A]):  A => M[B] =
    kernel.hyloMC[M, Either[B, *], F, A, B](_.fold(identity, algebra.apply).pure[M], coalgebra.run)

  def coelgot[F[_] : Functor, A, B](
    algebra: ElgotAlgebra[(A, *), F, B],
    coalgebra: Coalgebra[F, A]
  ): A => B =
    kernel.hyloC[(A, *), F, A, B](
      algebra.run,
      a => (a, coalgebra(a)))

  def coelgotM[M[_]: Monad, F[_]: Traverse, A, B](
                                                 algebra: ElgotAlgebraM[M, (A, *), F, B],
                                                 coalgebra: Coalgebra[F, A]): A => M[B] =
    kernel.hyloMC[M, (A, *), F, A, B](
      algebra.run,
      (a:A) => (a, coalgebra(a)).pure[M]
    )

  // Anamorphism that allows shortcuts
  def micro[F[_] : Functor, A, B](
    coalgebra: ElgotCoalgebra[Either[B, *], F, A]
  )(implicit embed: Embed[F, B]): A => B =
    elgot[F, A, B](embed.algebra, coalgebra)

}
