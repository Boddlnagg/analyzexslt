package analysis.domain.zipper

import analysis.domain.Lattice

abstract class ZList[T] {
  def ++(other: ZList[T])(implicit lat: Lattice[T]): ZList[T] = (this, other) match {
    case (ZBottom(), _) | (_, ZBottom()) => ZBottom()
    case (ZTop(), _) => ZTop()
    case (ZUnknownLength(elem), _) => ZUnknownLength(lat.join(elem, other.joinInner))
    case (ZCons(head, tail), _) => ZCons(head, (tail ++ other).asInstanceOf[ZListElement[T]])
    case (ZMaybeNil(head, tail), _) => other | (ZCons(head, tail) ++ other)
    case (ZNil(), _) => other
  }

  def |(other: ZList[T])(implicit lat: Lattice[T]): ZList[T] = (this, other) match {
    case (ZTop(), _) => ZTop()
    case (_, ZTop()) => ZTop()
    case (ZUnknownLength(elems), _) => lat.join(elems, other.joinInner) match {
      case x if ZList.isTop(x) => ZTop()
      case x => ZUnknownLength(x)
    }
    case (_, ZUnknownLength(elems)) => lat.join(elems, this.joinInner) match {
      case x if ZList.isTop(x) => ZTop()
      case x => ZUnknownLength(x)
    }
    case (ZBottom(), _) => other
    case (_, ZBottom()) => this
    case (ZCons(head1, tail1), ZCons(head2, tail2)) => ZCons(lat.join(head1, head2), (tail1 | tail2).asInstanceOf[ZListElement[T]])
    case (ZCons(head1, tail1), ZMaybeNil(head2, tail2)) => ZMaybeNil(lat.join(head1, head2), (tail1 | tail2).asInstanceOf[ZListElement[T]])
    case (ZMaybeNil(head1, tail1), ZCons(head2, tail2)) => ZMaybeNil(lat.join(head1, head2), (tail1 | tail2).asInstanceOf[ZListElement[T]])
    case (ZCons(head1, tail1), ZNil()) => ZMaybeNil(head1, tail1)
    case (ZNil(), ZCons(head1, tail1)) => ZMaybeNil(head1, tail1)
    case (ZMaybeNil(head1, tail1), ZMaybeNil(head2, tail2)) => ZMaybeNil(lat.join(head1, head2), (tail1 | tail2).asInstanceOf[ZListElement[T]])
    case (ZMaybeNil(_, _), ZNil()) => this
    case (ZNil(), ZMaybeNil(_, _)) => other
    case (ZNil(), ZNil()) => ZNil()
  }

  def joinInner(implicit lat: Lattice[T]): T = this match {
    case ZTop() => lat.top
    case ZUnknownLength(elems) => elems
    case ZBottom() => lat.bottom
    case ZCons(head, tail) => lat.join(head, tail.joinInner)
    case ZMaybeNil(head, tail) => lat.join(head, tail.joinInner)
    case ZNil() => lat.bottom
  }

  def &(other: ZList[T])(implicit lat: Lattice[T]): ZList[T] = (this, other) match {
    case (ZBottom(), _) => ZBottom()
    case (_, ZBottom()) => ZBottom()
    case (ZTop(), _) => other
    case (_, ZTop()) => this
    case (ZUnknownLength(elems), _) => other.meetInner(elems)
    case (_, ZUnknownLength(elems)) => this.meetInner(elems)
    case (ZCons(head1, tail1), ZCons(head2, tail2)) => (lat.meet(head1, head2), tail1 & tail2) match {
      case (_, ZBottom()) => ZBottom()
      case (head, _) if ZList.isBottom(head) => ZBottom()
      case (head, tail: ZListElement[T]) => ZCons(head, tail)
    }
    case (ZCons(head1, tail1), ZMaybeNil(head2, tail2)) => (lat.meet(head1, head2), tail1 & tail2) match {
      case (_, ZBottom()) => ZBottom()
      case (head, _) if ZList.isBottom(head) => ZBottom()
      case (head, tail: ZListElement[T]) => ZCons(head, tail)
    }
    case (ZMaybeNil(head1, tail1), ZCons(head2, tail2)) => (lat.meet(head1, head2), tail1 & tail2) match {
      case (_, ZBottom()) => ZBottom()
      case (head, _) if ZList.isBottom(head) => ZBottom()
      case (head, tail: ZListElement[T]) => ZCons(head, tail)
    }
    case (ZCons(_, _), ZNil()) => ZBottom()
    case (ZNil(), ZCons(_, _)) => ZBottom()
    case (ZMaybeNil(head1, tail1), ZMaybeNil(head2, tail2)) => (lat.meet(head1, head2), tail1 & tail2) match {
      case (_, ZBottom()) => ZBottom()
      case (head, _) if ZList.isBottom(head) => ZBottom()
      case (head, tail: ZListElement[T]) => ZMaybeNil(head, tail)
    }
    case (ZMaybeNil(_, _), ZNil()) => other
    case (ZNil(), ZMaybeNil(_, _)) => this
    case (ZNil(), ZNil()) => ZNil()
  }

  protected def meetInner(elems: T)(implicit lat: Lattice[T]): ZList[T] = this match {
    case ZTop() => if (ZList.isBottom(elems)) ZBottom() else ZUnknownLength(elems)
    case ZUnknownLength(e) => lat.meet(e, elems) match {
      case x if ZList.isBottom(x) => ZNil()
      case x => ZUnknownLength(x)
    }
    case ZBottom() => ZBottom()
    case ZCons(h, t) => (lat.meet(h, elems), t.meetInner(elems)) match {
      case (_, ZBottom()) => ZBottom()
      case (head, _) if ZList.isBottom(head) => ZBottom()
      case (head, tail: ZListElement[T]) => ZCons(head, tail)
    }
    case ZMaybeNil(h, t) => (lat.meet(h, elems), t.meetInner(elems)) match {
      case (_, ZBottom()) => ZNil()
      case (head, _) if ZList.isBottom(head) => ZNil()
      case (head, tail: ZListElement[T]) => ZMaybeNil(head, tail)
    }
    case ZNil() => ZNil()
  }

  def <=(other: ZList[T])(implicit lat: Lattice[T]): Boolean = (this, other) match {
    case (_, ZTop()) => true
    case (ZTop(), ZUnknownLength(elems)) => ZList.isTop(elems)
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

  def map[R](f: T => R)(implicit lat: Lattice[R]): ZList[R] = {
    def isBottom(v: R) = lat.lessThanOrEqual(v, lat.bottom)

    this match {
      case ZBottom() => ZBottom()
      case ZTop() => ZTop()
      case ZUnknownLength(elems) => ZUnknownLength(f(elems))
      case ZCons(head, tail) => (f(head), tail.map(f)) match {
        case (_, ZBottom()) => ZBottom()
        case (h, _) if isBottom(h) => ZBottom()
        case (h, t: ZListElement[R]) => ZCons(h, t)
      }
      case ZMaybeNil(head, tail) => (f(head), tail.map(f)) match {
        case (_, ZBottom()) => ZBottom()
        case (h, _) if isBottom(h) => ZBottom()
        case (h, t: ZListElement[R]) => ZMaybeNil(h, t)
      }
      case ZNil() => ZNil()
    }
  }

  def contains(element: T)(implicit lat: Lattice[T]): T = this match {
    case ZBottom() => lat.bottom
    case ZTop() => element
    case ZUnknownLength(elems) => lat.meet(element, elems)
    case ZCons(head, tail) => lat.join(lat.meet(head, element), tail.contains(element))
    case ZMaybeNil(head, tail) => lat.join(lat.meet(head, element), tail.contains(element))
    case ZNil() => lat.bottom
  }

  /** Gets the first element of the list. BOTTOM is returned if the list is BOTTOM or empty. */
  def first(implicit lat: Lattice[T]): T = this match {
    case ZBottom() => lat.bottom
    case ZTop() => lat.top
    case ZUnknownLength(elems) => elems
    case ZCons(head, tail) => head
    case ZMaybeNil(head, tail) => head
    case ZNil() => lat.bottom
  }
}

case class ZBottom[T]() extends ZList[T] // single element can not be BOTTOM, only the whole list
abstract class ZListElement[T] extends ZList[T]
case class ZTop[T]() extends ZListElement[T]
case class ZUnknownLength[T](elems: T) extends ZListElement[T] // elems should never be TOP
case class ZCons[T](head: T, tail: ZListElement[T]) extends ZListElement[T]
case class ZMaybeNil[T](head: T, tail: ZListElement[T]) extends ZListElement[T]
case class ZNil[T]() extends ZListElement[T]

object ZList {
  private def isBottom[T](v: T)(implicit lat: Lattice[T]) = lat.lessThanOrEqual(v, lat.bottom)
  private def isTop[T](v: T)(implicit lat: Lattice[T]) = lat.lessThanOrEqual(lat.top, v)

  def apply[T](list: List[T])(implicit lat: Lattice[T]): ZList[T] = list match {
    case head :: rest if isBottom(head) => ZBottom()
    case head :: rest if isTop(head) => ZTop()
    case head :: rest => apply(rest) match {
      case ZBottom() => ZBottom()
      case ZTop() => ZTop()
      case liftedRest => ZCons(head, liftedRest.asInstanceOf[ZListElement[T]])
    }
    case Nil => ZNil()
  }

  def join[T](lists: Traversable[ZList[T]])(implicit lat: Lattice[T]): ZList[T] =
    lists.fold(ZBottom[T]())(_ | _)

  def meet[T](lists: Traversable[ZList[T]])(implicit lat: Lattice[T]): ZList[T] =
    lists.fold(ZTop[T]())(_ & _)
}
