package arbo.data

import cats.{Applicative, Eval, Traverse}
import cats.data.NonEmptyList

sealed trait SellTree[+A, +O <: SellOrder] extends Serializable

object SellTree {
  case class RootNode[+A](children: NonEmptyList[A]) extends SellTree[A, Nothing]
  case class SellNode[+A, +O <: SellOrder](order: O, depth: Depth, children: NonEmptyList[A])
      extends SellTree[A, O]
  case class TerminalNode[+O <: SellOrder](order: O, depth: Depth) extends SellTree[Nothing, O]

  implicit def sellTreeTraverse[O <: SellOrder]: Traverse[SellTree[*, O]] =
    new Traverse[SellTree[*, O]] {
      override def traverse[G[_]: Applicative, A, B](fa: SellTree[A, O])(
          f: A => G[B]): G[SellTree[B, O]] =
        fa match {
          case tn: TerminalNode[O] => Applicative[G].pure(tn)
          case RootNode(ch) => Applicative[G].map(ch.traverse(f))(RootNode.apply)
          case SellNode(o, d, ch) => Applicative[G].map(ch.traverse(f))(SellNode(o, d, _))
        }

      override def foldLeft[A, B](fa: SellTree[A, O], b: B)(f: (B, A) => B): B = fa match {
        case _: TerminalNode[O] => b
        case RootNode(ch) => ch.foldLeft(b)(f)
        case SellNode(_, _, ch) => ch.foldLeft(b)(f)
      }

      override def foldRight[A, B](fa: SellTree[A, O], lb: Eval[B])(
          f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
        case _: TerminalNode[O] => lb
        case RootNode(ch) => ch.foldRight(lb)(f)
        case SellNode(_, _, ch) => ch.foldRight(lb)(f)
      }
    }
}
