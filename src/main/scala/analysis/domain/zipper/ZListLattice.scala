package analysis.domain.zipper

abstract class ZListLattice[+T]
case object ZBottom extends ZListLattice[Nothing]
case object ZTop extends ZListLattice[Nothing]

abstract class ZList[+T] extends ZListLattice[T]
case class ZCons[T](head: T, tail: ZList[T]) extends ZList[T]
abstract class ZMaybeNilList[+T] extends ZList[T]
case class ZMaybeNil[T](head: T, tail: ZMaybeNilList[T]) extends ZMaybeNilList[T]
case object ZNil extends ZMaybeNilList[Nothing]

object ZListLattice {
  def lift[T](list: List[T]): ZList[T] = {
    list match {
      case head :: rest => ZCons(head, lift(rest))
      case Nil => ZNil
    }
  }

  def join[T](left: ZListLattice[T], right: ZListLattice[T], joinElem: (T, T) => T): ZListLattice[T] = (left, right) match {
    case (ZTop, _) => ZTop
    case (_, ZTop) => ZTop
    case (ZBottom, _) => right
    case (_, ZBottom) => left
    case (ZCons(head1, tail1), ZCons(head2, tail2)) => ZCons(joinElem(head1, head2), join(tail1, tail2, joinElem).asInstanceOf[ZList[T]])
    case (ZCons(head1, tail1), ZMaybeNil(head2, tail2)) => ZMaybeNil(joinElem(head1, head2), join(tail1, tail2, joinElem).asInstanceOf[ZMaybeNilList[T]])
    case (ZMaybeNil(head1, tail1), ZCons(head2, tail2)) => ZMaybeNil(joinElem(head1, head2), join(tail1, tail2, joinElem).asInstanceOf[ZMaybeNilList[T]])
    case (ZCons(head1, tail1), ZNil) => ZMaybeNil(head1, join(tail1, ZNil, joinElem).asInstanceOf[ZMaybeNilList[T]])
    case (ZNil, ZCons(head1, tail1)) => ZMaybeNil(head1, join(tail1, ZNil, joinElem).asInstanceOf[ZMaybeNilList[T]])
    case (ZMaybeNil(head1, tail1), ZMaybeNil(head2, tail2)) => ZMaybeNil(joinElem(head1, head2), join(tail1, tail2, joinElem).asInstanceOf[ZMaybeNilList[T]])
    case (ZMaybeNil(_, _), ZNil) => left
    case (ZNil, ZMaybeNil(_, _)) => right
    case (ZNil, ZNil) => ZNil
  }

  def join[T](lists: Traversable[ZListLattice[T]], joinElem: (T, T) => T): ZListLattice[T] =
    lists.fold(ZBottom)((l, r) => join(l, r, joinElem))

  def concat[T](left: ZListLattice[T], right: ZListLattice[T], joinElem: (T, T) => T): ZListLattice[T] = (left, right) match {
    case (ZBottom, _) | (_, ZBottom) => ZBottom
    case (ZTop, _) | (_, ZTop) => ZTop
    case (ZCons(head, tail), _) => ZCons(head, concat(tail, right, joinElem).asInstanceOf[ZList[T]])
    case (ZMaybeNil(head, tail), _) => join(concat(ZCons(head, tail), right, joinElem), right, joinElem)
    case (ZNil, _) => right
  }
}
