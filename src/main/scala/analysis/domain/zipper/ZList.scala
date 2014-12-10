package analysis.domain.zipper

import analysis.domain.Lattice

abstract class ZList[T] {
  def ++(other: ZList[T])(implicit lat: Lattice[T]): ZList[T] = (this, other) match {
    case (ZBottom(), _) | (_, ZBottom()) => ZBottom()
    case (ZTop(), _) => ZTop()
    case (ZUnknownLength(elem), _) => ZUnknownLength(lat.join(elem, other.joinInner))
    case (ZCons(first, rest), _) => ZCons(first, (rest ++ other).asInstanceOf[ZListElement[T]])
    case (ZMaybeNil(first, rest), _) => other | (ZCons(first, rest) ++ other)
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
    case (ZCons(first1, rest1), ZCons(first2, rest2)) => ZCons(lat.join(first1, first2), (rest1 | rest2).asInstanceOf[ZListElement[T]])
    case (ZCons(first1, rest1), ZMaybeNil(first2, rest2)) => ZMaybeNil(lat.join(first1, first2), (rest1 | rest2).asInstanceOf[ZListElement[T]])
    case (ZMaybeNil(first1, rest1), ZCons(first2, rest2)) => ZMaybeNil(lat.join(first1, first2), (rest1 | rest2).asInstanceOf[ZListElement[T]])
    case (ZCons(first1, rest1), ZNil()) => ZMaybeNil(first1, rest1)
    case (ZNil(), ZCons(first1, rest1)) => ZMaybeNil(first1, rest1)
    case (ZMaybeNil(first1, rest1), ZMaybeNil(first2, rest2)) => ZMaybeNil(lat.join(first1, first2), (rest1 | rest2).asInstanceOf[ZListElement[T]])
    case (ZMaybeNil(_, _), ZNil()) => this
    case (ZNil(), ZMaybeNil(_, _)) => other
    case (ZNil(), ZNil()) => ZNil()
  }

  def joinInner(implicit lat: Lattice[T]): T = this match {
    case ZTop() => lat.top
    case ZUnknownLength(elems) => elems
    case ZBottom() => lat.bottom
    case ZCons(first, rest) => lat.join(first, rest.joinInner)
    case ZMaybeNil(first, rest) => lat.join(first, rest.joinInner)
    case ZNil() => lat.bottom
  }

  def &(other: ZList[T])(implicit lat: Lattice[T]): ZList[T] = (this, other) match {
    case (ZBottom(), _) => ZBottom()
    case (_, ZBottom()) => ZBottom()
    case (ZTop(), _) => other
    case (_, ZTop()) => this
    case (ZUnknownLength(elems), _) => other.meetInner(elems)
    case (_, ZUnknownLength(elems)) => this.meetInner(elems)
    case (ZCons(first1, rest1), ZCons(first2, rest2)) => (lat.meet(first1, first2), rest1 & rest2) match {
      case (_, ZBottom()) => ZBottom()
      case (first, _) if ZList.isBottom(first) => ZBottom()
      case (first, rest: ZListElement[T]) => ZCons(first, rest)
    }
    case (ZCons(first1, rest1), ZMaybeNil(first2, rest2)) => (lat.meet(first1, first2), rest1 & rest2) match {
      case (_, ZBottom()) => ZBottom()
      case (first, _) if ZList.isBottom(first) => ZBottom()
      case (first, rest: ZListElement[T]) => ZCons(first, rest)
    }
    case (ZMaybeNil(first1, rest1), ZCons(first2, rest2)) => (lat.meet(first1, first2), rest1 & rest2) match {
      case (_, ZBottom()) => ZBottom()
      case (first, _) if ZList.isBottom(first) => ZBottom()
      case (first, rest: ZListElement[T]) => ZCons(first, rest)
    }
    case (ZCons(_, _), ZNil()) => ZBottom()
    case (ZNil(), ZCons(_, _)) => ZBottom()
    case (ZMaybeNil(first1, rest1), ZMaybeNil(first2, rest2)) => (lat.meet(first1, first2), rest1 & rest2) match {
      case (_, ZBottom()) => ZBottom()
      case (first, _) if ZList.isBottom(first) => ZBottom()
      case (first, rest: ZListElement[T]) => ZMaybeNil(first, rest)
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
      case (first, _) if ZList.isBottom(first) => ZBottom()
      case (first, rest: ZListElement[T]) => ZCons(first, rest)
    }
    case ZMaybeNil(h, t) => (lat.meet(h, elems), t.meetInner(elems)) match {
      case (_, ZBottom()) => ZNil()
      case (first, _) if ZList.isBottom(first) => ZNil()
      case (first, rest: ZListElement[T]) => ZMaybeNil(first, rest)
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
    case (ZCons(first1, rest1), ZCons(first2, rest2)) => lat.lessThanOrEqual(first1, first2) && rest1 <= rest2
    case (ZCons(first1, rest1), ZMaybeNil(first2, rest2)) => lat.lessThanOrEqual(first1, first2) && rest1 <= rest2
    case (ZMaybeNil(first1, rest1), ZCons(first2, rest2)) => false
    case (ZCons(first1, rest1), ZNil()) => false
    case (ZNil(), ZCons(first1, rest1)) => false
    case (ZMaybeNil(first1, rest1), ZMaybeNil(first2, rest2)) => lat.lessThanOrEqual(first1, first2) && rest1 <= rest2
    case (ZMaybeNil(_, _), ZNil()) => false
    case (ZNil(), ZMaybeNil(_, _)) => true
    case (ZNil(), ZNil()) => true
  }

  /** This maps every node of the list using a given function.
    * As soon as the function returns BOTTOM for one input node, the resulting list will be BOTTOM. */
  // TODO: check if this function is only used correctly
  def map[R](f: T => R)(implicit latIn: Lattice[T], latOut: Lattice[R]): ZList[R] = {
    this match {
      case ZBottom() => ZBottom()
      case ZTop() => f(latIn.top) match {
        case h if ZList.isBottom(h) => ZBottom()
        case h if ZList.isTop(h) => ZTop()
        case h => ZUnknownLength(h)
      }
      case ZUnknownLength(elems) => f(elems) match {
        case h if ZList.isBottom(h) => ZBottom()
        case h if ZList.isTop(h) => ZTop()
        case h => ZUnknownLength(h)
      }
      case ZCons(first, rest) => (f(first), rest.map(f)) match {
        case (_, ZBottom()) => ZBottom()
        case (h, _) if ZList.isBottom(h) => ZBottom()
        case (h, t: ZListElement[R]) => ZCons(h, t)
      }
      case ZMaybeNil(first, rest) => (f(first), rest.map(f)) match {
        case (_, ZBottom()) => ZBottom()
        case (h, _) if ZList.isBottom(h) => ZBottom()
        case (h, t: ZListElement[R]) => ZMaybeNil(h, t)
      }
      case ZNil() => ZNil()
    }
  }

  def contains(element: T)(implicit lat: Lattice[T]): T = this match {
    case ZBottom() => lat.bottom
    case ZTop() => element
    case ZUnknownLength(elems) => lat.meet(element, elems)
    case ZCons(first, rest) => lat.join(lat.meet(first, element), rest.contains(element))
    case ZMaybeNil(first, rest) => lat.join(lat.meet(first, element), rest.contains(element))
    case ZNil() => lat.bottom
  }

  /** Gets the first element of the list. BOTTOM is returned if the list is BOTTOM or empty. */
  def first(implicit lat: Lattice[T]): T = this match {
    case ZBottom() => lat.bottom
    case ZTop() => lat.top
    case ZUnknownLength(elems) => elems
    case ZCons(first, rest) => first
    case ZMaybeNil(first, rest) => first
    case ZNil() => lat.bottom
  }

  def filter(predicate: T => T)(implicit lat: Lattice[T]): ZList[T] = this match {
    case ZBottom() => ZBottom()
    case ZTop() =>
      ZUnknownLength(predicate(lat.top))
    case ZUnknownLength(elems) =>
      val result = predicate(elems)
      if (lat.lessThanOrEqual(result, lat.bottom)) {
        // if result is BOTTOM, return an empty list
        ZNil()
      } else {
        ZUnknownLength(predicate(elems))
      }
    case ZCons(first, rest) =>
      val result = predicate(first)
      val restResult = rest.filter(predicate)
      if (lat.lessThanOrEqual(first, result)) { // first == result (because predicate application should never yield something greater)
        ZCons(first, restResult.asInstanceOf[ZListElement[T]]) // ... so nothing is filtered out
      } else if (lat.lessThanOrEqual(result, lat.bottom)) { // result == BOTTOM
        restResult // current node is filtered out completely
      } else {
        restResult | ZCons(result, restResult.asInstanceOf[ZListElement[T]])
      }
    case ZMaybeNil(first, rest) =>
      val result = predicate(first)
      val restResult = rest.filter(predicate)
      if (lat.lessThanOrEqual(first, result)) { // first == result (because predicate application should never yield something greater)
        ZMaybeNil(first, restResult.asInstanceOf[ZListElement[T]]) // ... so nothing is filtered out
      } else if (lat.lessThanOrEqual(result, lat.bottom)) { // result == BOTTOM
        restResult // current node is filtered out completely
      } else {
        restResult | ZMaybeNil(result, restResult.asInstanceOf[ZListElement[T]])
      }
    case ZNil() => ZNil()
  }

  def takeWhile(predicate: T => T)(implicit lat: Lattice[T]): ZList[T] = this match {
    case ZBottom() => ZBottom()
    case ZTop() =>
      ZUnknownLength(predicate(lat.top))
    case ZUnknownLength(elems) =>
      val result = predicate(elems)
      if (lat.lessThanOrEqual(result, lat.bottom)) {
        // if result is BOTTOM, return an empty list
        ZNil()
      } else {
        ZUnknownLength(predicate(elems))
      }
    case ZCons(first, rest) =>
      val result = predicate(first)
      val restResult = rest.takeWhile(predicate)
      if (lat.lessThanOrEqual(first, result)) { // first == result (because predicate application should never yield something greater)
        ZCons(first, restResult.asInstanceOf[ZListElement[T]]) // ... so nothing is filtered out
      } else if (lat.lessThanOrEqual(result, lat.bottom)) { // result == BOTTOM
        ZNil() // predicate definitely doesn't hold for current node, so the result list ends here
      } else {
        ZMaybeNil(result, restResult.asInstanceOf[ZListElement[T]])
      }
    case ZMaybeNil(first, rest) =>
      val result = predicate(first)
      val restResult = rest.takeWhile(predicate)
      if (lat.lessThanOrEqual(result, lat.bottom)) { // result == BOTTOM
        ZNil() // predicate definitely doesn't hold for current node, so the result list ends here
      } else {
        // we don't know if the predicate holds for the current node, so the result list might or might not end here
        ZMaybeNil(result, restResult.asInstanceOf[ZListElement[T]])
      }
    case ZNil() => ZNil()
  }

  override def toString: String = {
    if (this.isInstanceOf[ZBottom[T]]) return "[BOTTOM]"

    val b = new StringBuilder("[")
    def appendElement(elem: ZListElement[T]): Unit = elem match {
      case ZTop() => b.append("TOP]")
      case ZUnknownLength(elems) => b.append(elems.toString).append("*]")
      case ZCons(first, ZNil()) => b.append(first.toString).append("]")
      case ZCons(first, rest) =>
        b.append(first.toString).append(",")
        appendElement(rest)
      case ZMaybeNil(first, ZNil()) => b.append("NIL+").append(first.toString).append("]")
      case ZMaybeNil(first, rest) =>
        b.append("NIL+").append(first.toString).append(",")
        appendElement(rest)
      case ZNil() => b.append("]")
    }
    appendElement(this.asInstanceOf[ZListElement[T]])
    b.toString()
  }
}

case class ZBottom[T]() extends ZList[T] // single element can not be BOTTOM, only the whole list
abstract class ZListElement[T] extends ZList[T]
case class ZTop[T]() extends ZListElement[T]
case class ZUnknownLength[T](elems: T) extends ZListElement[T] // elems should never be TOP
case class ZCons[T](first: T, rest: ZListElement[T]) extends ZListElement[T]
case class ZMaybeNil[T](first: T, rest: ZListElement[T]) extends ZListElement[T]
case class ZNil[T]() extends ZListElement[T]

object ZList {
  private def isBottom[T](v: T)(implicit lat: Lattice[T]) = lat.lessThanOrEqual(v, lat.bottom)
  private def isTop[T](v: T)(implicit lat: Lattice[T]) = lat.lessThanOrEqual(lat.top, v)

  def apply[T](list: List[T])(implicit lat: Lattice[T]): ZList[T] = list match {
    case first :: rest if isBottom(first) => ZBottom()
    case first :: rest => apply(rest) match {
      case ZBottom() => ZBottom()
      case liftedRest => ZCons(first, liftedRest.asInstanceOf[ZListElement[T]])
    }
    case Nil => ZNil()
  }

  def apply[T](elems: T*)(implicit lat: Lattice[T]): ZList[T] = apply(elems.toList)

  def joinAll[T](lists: Traversable[ZList[T]])(implicit lat: Lattice[T]): ZList[T] =
    lists.fold(ZBottom[T]())(_ | _)

  def meetAll[T](lists: Traversable[ZList[T]])(implicit lat: Lattice[T]): ZList[T] =
    lists.fold(ZTop[T]())(_ & _)
}
