package evaluation

import analysis.domain.zipper._
import org.scalatest.FunSuite

class ZipperSuite extends FunSuite {
  def joinElem(s1: Set[Int], s2: Set[Int]) = s1 union s2

  test("Lift list") {
    assertResult(ZCons(Set(1), ZCons(Set(2), ZCons(Set(3), ZNil)))) {
      ZListLattice.lift(List(Set(1), Set(2), Set(3)))
    }
  }

  test("Join list") {
    val l1 = ZListLattice.lift(List(Set(1), Set(2), Set(3)))
    val l2 = ZListLattice.lift(List(Set(4), Set(5)))
    val l3 = ZListLattice.lift(List(Set(6)))

    assertResult(ZCons(Set(1, 4),ZCons(Set(2, 5),ZMaybeNil(Set(3),ZNil)))) {
      ZListLattice.join(l1, l2, joinElem)
    }

    assertResult(l1) { ZListLattice.join(l1, ZBottom, joinElem) }
    assertResult(l2) { ZListLattice.join(ZBottom, l2, joinElem) }
    assertResult(ZTop) { ZListLattice.join(l1, ZTop, joinElem) }
    assertResult(ZTop) { ZListLattice.join(ZTop, l2, joinElem) }
    assertResult(ZTop) { ZListLattice.join(ZBottom, ZTop, joinElem) }

    assertResult(ZCons(Set(1, 4, 6),ZMaybeNil(Set(2, 5),ZMaybeNil(Set(3),ZNil)))) {
      ZListLattice.join(List(l1, l2, l3), joinElem)
    }
  }

  test("Concat lists") {
    val l1 = ZListLattice.lift(List(Set(1), Set(2), Set(3)))
    val l2 = ZListLattice.lift(List(Set(1), Set(2)))

    val l12 = ZListLattice.join(l1, l2, joinElem)

    assertResult(ZCons(Set(1), ZCons(Set(2), ZCons(Set(1), ZCons(Set(2), ZNil))))) {
      ZListLattice.concat(l2, l2, joinElem)
    }

    assertResult(ZCons(Set(1),ZCons(Set(2),ZCons(Set(3, 1),ZCons(Set(1, 2),ZMaybeNil(Set(2),ZNil)))))) {
      ZListLattice.concat(l12, l2, joinElem)
    }

    assertResult(ZCons(Set(1),ZCons(Set(2),ZCons(Set(3, 1),ZCons(Set(1, 2),ZMaybeNil(Set(2, 3),ZMaybeNil(Set(3),ZNil))))))) {
      ZListLattice.concat(l12, l12, joinElem)
    }

    assertResult(ZTop) { ZListLattice.concat(l12, ZTop, joinElem) }
    assertResult(ZTop) { ZListLattice.concat(ZTop, l12, joinElem) }
    assertResult(ZBottom) { ZListLattice.concat(l12, ZBottom, joinElem) }
    assertResult(ZBottom) { ZListLattice.concat(ZBottom, l12, joinElem) }
    assertResult(ZBottom) { ZListLattice.concat(ZTop, ZBottom, joinElem) }
    assertResult(ZBottom) { ZListLattice.concat(ZBottom, ZTop, joinElem) }
  }

}
