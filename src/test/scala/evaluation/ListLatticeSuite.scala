package evaluation

import analysis.domain.zipper._
import org.scalatest.FunSuite

class ListLatticeSuite extends FunSuite {
  implicit object IntSetLattice extends Lattice[Option[Set[Int]]] {
    def top = None
    def bottom = Some(Set())
    def join(left: Option[Set[Int]], right: Option[Set[Int]]): Option[Set[Int]] = (left, right) match {
      case (None, _) | (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1 | s2)
    }
    def meet(left: Option[Set[Int]], right: Option[Set[Int]]): Option[Set[Int]] = (left, right) match {
      case (None, _) => right
      case (_, None) => left
      case (Some(s1), Some(s2)) => Some(s1 & s2)
    }
    def lessThanOrEqual(left: Option[Set[Int]], right: Option[Set[Int]]): Boolean = (left, right) match {
      case (_, None) => true
      case (None, _) => false
      case (Some(s1), Some(s2)) => s1.subsetOf(s2)
    }
  }

  def lift(v: Int*): Option[Set[Int]] = Some(v.toSet)

  test("Lift") {
    assertResult(ZCons(lift(1), ZCons(lift(2), ZCons(lift(3), ZNil())))) {
      ZListLattice(List(lift(1), lift(2), lift(3)))
    }
  }

  test("Join") {
    val l1 = ZListLattice(List(lift(1), lift(2), lift(3)))
    val l2 = ZListLattice(List(lift(4), lift(5)))
    val l3 = ZListLattice(List(lift(6)))

    assertResult(ZCons(lift(1, 4),ZCons(lift(2, 5),ZMaybeNil(lift(3),ZNil())))) {
      l1 | l2
    }

    assertResult(l1) { l1 | ZBottom() }
    assertResult(l2) { ZBottom() | l2 }
    assertResult(ZTop()) { l1 | ZTop() }
    assertResult(ZTop()) { ZTop() | l2 }
    assertResult(ZTop()) { ZBottom() | ZTop() }

    assertResult(ZCons(lift(1, 4, 6),ZMaybeNil(lift(2, 5),ZMaybeNil(lift(3),ZNil())))) {
      ZListLattice.join(List(l1, l2, l3))
    }
  }

  test("Concat") {
    val l1 = ZListLattice(List(lift(1), lift(2), lift(3)))
    val l2 = ZListLattice(List(lift(1), lift(2)))

    val l12 = l1 | l2

    assertResult(ZCons(lift(1), ZCons(lift(2), ZCons(lift(1), ZCons(lift(2), ZNil()))))) {
      l2 ++ l2
    }

    assertResult(ZCons(lift(1),ZCons(lift(2),ZCons(lift(3, 1),ZCons(lift(1, 2),ZMaybeNil(lift(2),ZNil())))))) {
      l12 ++ l2
    }

    assertResult(ZCons(lift(1),ZCons(lift(2),ZCons(lift(3, 1),ZCons(lift(1, 2),ZMaybeNil(lift(2, 3),ZMaybeNil(lift(3),ZNil()))))))) {
      l12 ++ l12
    }

    assertResult(ZTop()) { l12 ++ ZTop() }
    assertResult(ZTop()) { ZTop() ++ l12 }
    assertResult(ZBottom()) { l12 ++ ZBottom() }
    assertResult(ZBottom()) { ZBottom() ++ l12 }
    assertResult(ZBottom()) { ZTop() ++ ZBottom() }
    assertResult(ZBottom()) { ZBottom() ++ ZTop() }
  }

  test("Compare (<=)") {
    val l1 = ZListLattice(List(lift(1), lift(2), lift(3)))
    val l2 = ZListLattice(List(lift(1), lift(2)))
    val l12 = l1 | l2
    val l123 = l1 | l2 | ZNil()

    assertResult(false) { l1 <= l2 }
    assertResult(false) { l2 <= l1 }
    assertResult(false) { ZTop() <= l1 }
    assertResult(true) { l1 <= ZTop() }
    assertResult(true) { ZBottom() <= l1 }
    assertResult(false) { l1 <= ZBottom() }
    assertResult(true) { l1 <= l12 }
    assertResult(false) { l12 <= l1 }
    assertResult(true) { l2 <= l12 }
    assertResult(false) { l12 <= l2 }
    assertResult(false) { ZNil() <= l12 }
    assertResult(true) { ZNil() <= l123 }
  }
}
