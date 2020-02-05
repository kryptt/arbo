package arbo.data

import cats.Functor
import cats.data.NonEmptyList

sealed trait SellTree[+A]

object SellTree {
  case class SellNode[+A](order: SellOrder, depth: Depth, children: NonEmptyList[A]) extends SellTree[A]
  case class TerminalNode(order: SellOrder, depth: Depth) extends SellTree[Nothing]

  implicit val SellTreeFunctor = new Functor[SellTree] {
    def map[A, B](st: SellTree[A])(f: A => B) = st match {
      case tn : TerminalNode => tn
      case SellNode(order, depth, children) =>
        SellNode(order, depth, children.map(f))
    }
  }
}
