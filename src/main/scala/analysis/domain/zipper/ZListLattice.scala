package analysis.domain.zipper

import analysis.domain.Lattice

abstract class ZListLattice[T] {
  def ++(other: ZListLattice[T])(implicit lat: Lattice[T]): ZListLattice[T] = (this, other) match {
    case (ZBottom(), _) | (_, ZBottom()) => ZBottom()
    case (ZTop(), _) | (_, ZTop()) => ZTop()
    case (ZCons(head, tail), _) => ZCons(head, (tail ++ other).asInstanceOf[ZList[T]])
    case (ZMaybeNil(head, tail), _) => other | (ZCons(head, tail) ++ other)
    case (ZNil(), _) => other
  }

  def |(other: ZListLattice[T])(implicit lat: Lattice[T]): ZListLattice[T] = (this, other) match {
    case (ZTop(), _) => ZTop()
    case (_, ZTop()) => ZTop()
    case (ZBottom(), _) => other
    case (_, ZBottom()) => this
    case (ZCons(head1, tail1), ZCons(head2, tail2)) => ZCons(lat.join(head1, head2), (tail1 | tail2).asInstanceOf[ZList[T]])
    case (ZCons(head1, tail1), ZMaybeNil(head2, tail2)) => ZMaybeNil(lat.join(head1, head2), (tail1 | tail2).asInstanceOf[ZMaybeNilList[T]])
    case (ZMaybeNil(head1, tail1), ZCons(head2, tail2)) => ZMaybeNil(lat.join(head1, head2), (tail1 | tail2).asInstanceOf[ZMaybeNilList[T]])
    case (ZCons(head1, tail1), ZNil()) => ZMaybeNil(head1, (tail1 | ZNil()).asInstanceOf[ZMaybeNilList[T]])
    case (ZNil(), ZCons(head1, tail1)) => ZMaybeNil(head1, (tail1 | ZNil()).asInstanceOf[ZMaybeNilList[T]])
    case (ZMaybeNil(head1, tail1), ZMaybeNil(head2, tail2)) => ZMaybeNil(lat.join(head1, head2), (tail1 | tail2).asInstanceOf[ZMaybeNilList[T]])
    case (ZMaybeNil(_, _), ZNil()) => this
    case (ZNil(), ZMaybeNil(_, _)) => other
    case (ZNil(), ZNil()) => ZNil()
  }

  def &(other: ZListLattice[T])(implicit lat: Lattice[T]): ZListLattice[T] = {
    def isBottom(v: T) = lat.lessThanOrEqual(v, lat.bottom)

    (this, other) match {
      case (ZBottom(), _) => ZBottom()
      case (_, ZBottom()) => ZBottom()
      case (ZTop(), _) => other
      case (_, ZTop()) => this
      case (ZCons(head1, tail1), ZCons(head2, tail2)) => (lat.meet(head1, head2), tail1 & tail2) match {
        case (_, ZBottom()) => ZBottom()
        case (head, _) if isBottom(head) => ZBottom()
        case (head, tail) => ZCons(head, tail.asInstanceOf[ZList[T]])
      }
      case (ZCons(head1, tail1), ZMaybeNil(head2, tail2)) => (lat.meet(head1, head2), tail1 & tail2) match {
        case (_, ZBottom()) => ZBottom()
        case (head, _) if isBottom(head) => ZBottom()
        case (head, tail) => ZCons(head, tail.asInstanceOf[ZList[T]])
      }
      case (ZMaybeNil(head1, tail1), ZCons(head2, tail2)) => (lat.meet(head1, head2), tail1 & tail2) match {
        case (_, ZBottom()) => ZBottom()
        case (head, _) if isBottom(head) => ZBottom()
        case (head, tail) => ZCons(head, tail.asInstanceOf[ZList[T]])
      }
      case (ZCons(_, _), ZNil()) => ZBottom()
      case (ZNil(), ZCons(_, _)) => ZBottom()
      case (ZMaybeNil(head1, tail1), ZMaybeNil(head2, tail2)) => (lat.meet(head1, head2), tail1 & tail2) match {
        case (_, ZBottom()) => ZBottom()
        case (head, _) if isBottom(head) => ZBottom()
        case (head, tail) => ZMaybeNil(head, tail.asInstanceOf[ZMaybeNilList[T]])
      }
      case (ZMaybeNil(_, _), ZNil()) => other
      case (ZNil(), ZMaybeNil(_, _)) => this
      case (ZNil(), ZNil()) => ZNil()
    }
  }

  def <=(other: ZListLattice[T])(implicit lat: Lattice[T]): Boolean = (this, other) match {
    case (_, ZTop()) => true
    case (ZTop(), _) => false // second argument is not TOP because that was handled in the previous case
    case (ZBottom(), _) => true
    case (_, ZBottom()) => false // first argument is not BOTTOM because that was handled in the previous case
    case (ZCons(head1, tail1), ZCons(head2, tail2)) => lat.lessThanOrEqual(head1, head2) && tail1 <= tail2
    case (ZCons(head1, tail1), ZMaybeNil(head2, tail2)) => lat.lessThanOrEqual(head1, head2) && tail1 <= tail2
    case (ZMaybeNil(head1, tail1), ZCons(head2, tail2)) => false
    case (ZCons(head1, tail1), ZNil()) => false
    case (ZNil(), ZCons(head1, tail1)) => false
    case (ZMaybeNil(head1, tail1), ZMaybeNil(head2, tail2)) => lat.lessThanOrEqual(head1, head2) && tail1 <= tail2
    case (ZMaybeNil(_, _), ZNil()) => false
    case (ZNil(), ZMaybeNil(_, _)) => true
    case (ZNil(), ZNil()) => true
  }

  def map[R](f: T => R)(implicit lat: Lattice[R]): ZListLattice[R] = {
    def isBottom(v: R) = lat.lessThanOrEqual(v, lat.bottom)

    this match {
      case ZBottom() => ZBottom()
      case ZTop() => ZTop()
      case ZCons(head, tail) => (f(head), tail.map(f)) match {
        case (_, ZBottom()) => ZBottom()
        case (h, _) if isBottom(h) => ZBottom()
        case (h, t) => ZCons(h, t.asInstanceOf[ZList[R]])
      }
      case ZMaybeNil(head, tail) => (f(head), tail.map(f)) match {
        case (_, ZBottom()) => ZBottom()
        case (h, _) if isBottom(h) => ZBottom()
        case (h, t) => ZMaybeNil(h, t.asInstanceOf[ZMaybeNilList[R]])
      }
      case ZNil() => ZNil()
    }
  }
}

case class ZBottom[T]() extends ZListLattice[T]
case class ZTop[T]() extends ZListLattice[T]

abstract class ZList[T] extends ZListLattice[T]
case class ZCons[T](head: T, tail: ZList[T]) extends ZList[T]
abstract class ZMaybeNilList[T] extends ZList[T]
case class ZMaybeNil[T](head: T, tail: ZMaybeNilList[T]) extends ZMaybeNilList[T]
case class ZNil[T]() extends ZMaybeNilList[T]

object ZListLattice {
  def apply[T](list: List[T]): ZList[T] = {
    list match {
      case head :: rest => ZCons(head, apply(rest))
      case Nil => ZNil()
    }
  }

  def join[T](lists: Traversable[ZListLattice[T]])(implicit lat: Lattice[T]): ZListLattice[T] =
    lists.fold(ZBottom())(_ | _)

  def meet[T](lists: Traversable[ZListLattice[T]])(implicit lat: Lattice[T]): ZListLattice[T] =
    lists.fold(ZTop())(_ & _)
}
