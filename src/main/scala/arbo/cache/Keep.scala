package arbo
package cache

import scalacache._
import scalacache.caffeine._

import cats.effect.{Async, Resource}
import cats.effect.concurrent.{Semaphore, Ref}

import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.concurrent.duration._

object Keep {

  def keep[F[_]: Async, A](time: FiniteDuration)(fa: F[A]): F[F[A]] = {

    def eval: F[(Long, A)] = fa.map(a => (System.nanoTime(), a))

    def maintain(ref: Ref[F, (Long, A)]): F[A] = ref.get.flatMap {
      case (tm, _) if tm + time.toNanos < System.nanoTime() =>
        eval.flatTap(ref.set).map(_._2)
      case (_, a) =>
        Async[F].pure(a)
    }

    def run(sem: Semaphore[F], ref: Ref[F, (Long, A)]): F[A] =
      sem.withPermit(maintain(ref))

    val sem = Semaphore.uncancelable[F](1)
    val ref = eval.flatMap(ini => Ref.of(ini))

    (sem, ref).mapN(run)
  }

  def cache[F[_]: Async, A]: Resource[F, Cache[A]] = {
    import CatsEffect.modes._
    Resource.make(Async[F].delay(CaffeineCache[A]))(_.close().void)
  }

}
