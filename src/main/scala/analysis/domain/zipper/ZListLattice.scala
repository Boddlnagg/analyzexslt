package analysis.domain.zipper

import analysis.domain.Lattice

abstract class ZListLattice[T] {
  def ++(other: ZListLattice[T])(implicit lat: Lattice[T]): ZListLattice[T] = (this, other) match {
    case (ZBottom(), _) | (_, ZBottom()) => ZBottom()
    case (ZTop(), _) => ZTop()
    case (ZUnknownLength(elem), _) => ZUnknownLength(lat.join(elem, other.joinInner))
    case (ZCons(head, tail), _) => ZCons(head, (tail ++ other).asInstanceOf[ZList[T]])
    case (ZMaybeNil(head, tail), _) => other | (ZCons(head, tail) ++ other)
    case (ZNil(), _) => other
  }

  private def isBottom(v: T)(implicit lat: Lattice[T]) = lat.lessThanOrEqual(v, lat.bottom)
  private def isTop(v: T)(implicit lat: Lattice[T]) = lat.lessThanOrEqual(lat.top, v)

  def |(other: ZListLattice[T])(implicit lat: Lattice[T]): ZListLattice[T] = (this, other) match {
    case (ZTop(), _) => ZTop()
    case (_, ZTop()) => ZTop()
    case (ZUnknownLength(elems), _) => lat.join(elems, other.joinInner) match {
      case x if isTop(x) => ZTop()
      case x => ZUnknownLength(x)
    }
    case (_, ZUnknownLength(elems)) => lat.join(elems, this.joinInner) match {
      case x if isTop(x) => ZTop()
      case x => ZUnknownLength(x)
    }
    case (ZBottom(), _) => other
    case (_, ZBottom()) => this
    case (ZCons(head1, tail1), ZCons(head2, tail2)) => ZCons(lat.join(head1, head2), (tail1 | tail2).asInstanceOf[ZList[T]])
    case (ZCons(head1, tail1), ZMaybeNil(head2, tail2)) => ZMaybeNil(lat.join(head1, head2), (tail1 | tail2).asInstanceOf[ZList[T]])
    case (ZMaybeNil(head1, tail1), ZCons(head2, tail2)) => ZMaybeNil(lat.join(head1, head2), (tail1 | tail2).asInstanceOf[ZList[T]])
    case (ZCons(head1, tail1), ZNil()) => ZMaybeNil(head1, tail1)
    case (ZNil(), ZCons(head1, tail1)) => ZMaybeNil(head1, tail1)
    case (ZMaybeNil(head1, tail1), ZMaybeNil(head2, tail2)) => ZMaybeNil(lat.join(head1, head2), (tail1 | tail2).asInstanceOf[ZList[T]])
    case (ZMaybeNil(_, _), ZNil()) => this
    case (ZNil(), ZMaybeNil(_, _)) => other
    case (ZNil(), ZNil()) => ZNil()
  }

  protected def joinInner(implicit lat: Lattice[T]): T = this match {
    case ZTop() => lat.top
    case ZUnknownLength(elems) => elems
    case ZBottom() => lat.bottom
    case ZCons(head, tail) => lat.join(head, tail.joinInner)
    case ZMaybeNil(head, tail) => lat.join(head, tail.joinInner)
    case ZNil() => lat.bottom
  }

  def &(other: ZListLattice[T])(implicit lat: Lattice[T]): ZListLattice[T] = (this, other) match {
    case (ZBottom(), _) => ZBottom()
    case (_, ZBottom()) => ZBottom()
    case (ZTop(), _) => other
    case (_, ZTop()) => this
    case (ZUnknownLength(elems), _) => other.meetInner(elems)
    case (_, ZUnknownLength(elems)) => this.meetInner(elems)
    case (ZCons(head1, tail1), ZCons(head2, tail2)) => (lat.meet(head1, head2), tail1 & tail2) match {
      case (_, ZBottom()) => ZBottom()
      case (head, _) if isBottom(head) => ZBottom()
      case (head, tail: ZList[T]) => ZCons(head, tail)
    }
    case (ZCons(head1, tail1), ZMaybeNil(head2, tail2)) => (lat.meet(head1, head2), tail1 & tail2) match {
      case (_, ZBottom()) => ZBottom()
      case (head, _) if isBottom(head) => ZBottom()
      case (head, tail: ZList[T]) => ZCons(head, tail)
    }
    case (ZMaybeNil(head1, tail1), ZCons(head2, tail2)) => (lat.meet(head1, head2), tail1 & tail2) match {
      case (_, ZBottom()) => ZBottom()
      case (head, _) if isBottom(head) => ZBottom()
      case (head, tail: ZList[T]) => ZCons(head, tail)
    }
    case (ZCons(_, _), ZNil()) => ZBottom()
    case (ZNil(), ZCons(_, _)) => ZBottom()
    case (ZMaybeNil(head1, tail1), ZMaybeNil(head2, tail2)) => (lat.meet(head1, head2), tail1 & tail2) match {
      case (_, ZBottom()) => ZBottom()
      case (head, _) if isBottom(head) => ZBottom()
      case (head, tail: ZList[T]) => ZMaybeNil(head, tail)
    }
    case (ZMaybeNil(_, _), ZNil()) => other
    case (ZNil(), ZMaybeNil(_, _)) => this
    case (ZNil(), ZNil()) => ZNil()
  }

  protected def meetInner(elems: T)(implicit lat: Lattice[T]): ZListLattice[T] = this match {
    case ZTop() => if (isBottom(elems)) ZBottom() else ZUnknownLength(elems)
    case ZUnknownLength(e) => lat.meet(e, elems) match {
      case x if isBottom(x) => ZNil()
      case x => ZUnknownLength(x)
    }
    case ZBottom() => ZBottom()
    case ZCons(h, t) => (lat.meet(h, elems), t.meetInner(elems)) match {
      case (_, ZBottom()) => ZBottom()
      case (head, _) if isBottom(head) => ZBottom()
      case (head, tail: ZList[T]) => ZCons(head, tail)
    }
    case ZMaybeNil(h, t) => (lat.meet(h, elems), t.meetInner(elems)) match {
      case (_, ZBottom()) => ZNil()
      case (head, _) if isBottom(head) => ZNil()
      case (head, tail: ZList[T]) => ZMaybeNil(head, tail)
    }
    case ZNil() => ZNil()
  }

  def <=(other: ZListLattice[T])(implicit lat: Lattice[T]): Boolean = (this, other) match {
    case (_, ZTop()) => true
    case (ZTop(), ZUnknownLength(elems)) => isTop(elems)
    case (ZTop(), _) => false
    case (_, ZUnknownLength(elems)) => lat.lessThanOrEqual(this.joinInner, elems)
    case (ZUnknownLength(elems), _) => false // second argument is not TOP because that was handled in the previous case
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
      case ZUnknownLength(elems) => ZUnknownLength(f(elems))
      case ZCons(head, tail) => (f(head), tail.map(f)) match {
        case (_, ZBottom()) => ZBottom()
        case (h, _) if isBottom(h) => ZBottom()
        case (h, t: ZList[R]) => ZCons(h, t)
      }
      case ZMaybeNil(head, tail) => (f(head), tail.map(f)) match {
        case (_, ZBottom()) => ZBottom()
        case (h, _) if isBottom(h) => ZBottom()
        case (h, t: ZList[R]) => ZMaybeNil(h, t)
      }
      case ZNil() => ZNil()
    }
  }
}

case class ZBottom[T]() extends ZListLattice[T] // single element can not be BOTTOM, only the whole list
abstract class ZList[T] extends ZListLattice[T]
case class ZTop[T]() extends ZList[T]
case class ZUnknownLength[T](elems: T) extends ZList[T] // elems should never be TOP
case class ZCons[T](head: T, tail: ZList[T]) extends ZList[T]
case class ZMaybeNil[T](head: T, tail: ZList[T]) extends ZList[T]
case class ZNil[T]() extends ZList[T]

object ZListLattice {
  def apply[T](list: List[T]): ZList[T] = {
    list match {
      case head :: rest => ZCons(head, apply(rest))
      case Nil => ZNil()
    }
  }

  def join[T](lists: Traversable[ZListLattice[T]])(implicit lat: Lattice[T]): ZListLattice[T] =
    lists.fold(ZBottom[T]())(_ | _)

  def meet[T](lists: Traversable[ZListLattice[T]])(implicit lat: Lattice[T]): ZListLattice[T] =
    lists.fold(ZTop[T]())(_ & _)
}
