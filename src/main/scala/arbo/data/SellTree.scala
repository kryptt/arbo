package arbo.data

import cats.{Applicative, Eval, Traverse}
import cats.data.NonEmptyList

sealed trait SellTree[+A]

object SellTree {
  case class SellNode[+A](order: SellOrder, depth: Depth, children: NonEmptyList[A]) extends SellTree[A]
  case class TerminalNode(order: SellOrder, depth: Depth) extends SellTree[Nothing]

  implicit val SellTreeTraverse: Traverse[SellTree] = new Traverse[SellTree] {
    override def traverse[G[_]: Applicative, A, B](fa: SellTree[A])(f: A => G[B]): G[SellTree[B]] = fa match {
      case tn:TerminalNode => Applicative[G].pure(tn)
      case SellNode(o, d, ch) => Applicative[G].map(ch.traverse(f))(SellNode(o, d, _))
    }


    override def foldLeft[A, B](fa: SellTree[A], b: B)(f: (B, A) => B): B = fa match {
      case _:TerminalNode => b
      case SellNode(_, _, ch) => ch.foldLeft(b)(f)
    }

    override def foldRight[A, B](fa: SellTree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case _:TerminalNode => lb
      case SellNode(_, _, ch) => ch.foldRight(lb)(f)
    }
  }
}
