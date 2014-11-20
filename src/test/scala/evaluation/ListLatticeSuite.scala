package evaluation

import analysis.domain.zipper._
import org.scalatest.FunSuite

class ListLatticeSuite extends FunSuite {
  type L = Option[Set[Int]]

  def lift(v: Int*): L = Some(v.toSet)

  test("Lift") {
    assertResult(ZCons(lift(1), ZCons(lift(2), ZCons(lift(3), ZNil())))) {
      ZListLattice(List(lift(1), lift(2), lift(3)))
    }
  }

  test("Join") {
    val l1 = ZListLattice(List(lift(1), lift(2), lift(3)))
    val l2 = ZListLattice(List(lift(4), lift(5)))
    val l3 = ZListLattice(List(lift(6)))
    val l4 = ZCons(lift(4), ZCons(lift(5), ZTop()))
    val l5 = ZCons(lift(1), ZUnknownLength(lift(4,5,6)))

    assertResult(ZCons(lift(1, 4),ZCons(lift(2, 5),ZMaybeNil(lift(3),ZNil())))) {
      l1 | l2
    }

    assertResult(l1) { l1 | ZBottom() }
    assertResult(l2) { ZBottom() | l2 }
    assertResult(ZTop()) { l1 | ZTop() }
    assertResult(ZTop()) { ZTop() | l2 }
    assertResult(ZTop()) { ZBottom() | ZTop() }

    assertResult(ZCons(Some(Set(1, 6)),ZMaybeNil(Some(Set(2)),ZCons(Some(Set(3)),ZNil())))) { l1 | l3 }

    assertResult(ZCons(lift(1, 4, 6),ZMaybeNil(lift(2, 5),ZMaybeNil(lift(3),ZNil())))) {
      ZListLattice.join(List(l1, l2, l3))
    }

    assertResult(ZCons(lift(1, 4), ZCons(lift(2, 5), ZTop()))) { l1 | l4 }
    assertResult(ZCons(lift(1), ZUnknownLength(lift(2,3,4,5,6)))) { l1 | l5 }
  }

  test("Meet") {
    val l1 = ZListLattice(List(lift(1), lift(2), lift(3)))
    val l2 = ZListLattice(List(lift(4), lift(5)))
    val l3 = ZListLattice(List(lift(6)))
    val l12 = l1 | l2
    val l12Nil = l1 | l2 | ZNil()
    val l1b = ZListLattice(List(lift(1), lift(2), lift(4)))
    val l13 = l1 | l3
    val l4 = ZCons(lift(1,4), ZUnknownLength(lift(2,3)))
    val l5 = ZCons(lift(1,4), ZUnknownLength(lift(5)))

    assertResult(ZBottom()) { l1 & l2 }
    assertResult(l1) { l1 & ZTop() }
    assertResult(l2) { ZTop() & l2 }
    assertResult(ZBottom()) { l2 & ZBottom() }
    assertResult(ZBottom()) { ZBottom() & l1 }
    assertResult(ZBottom()) { ZBottom() & ZTop() }
    assertResult(l1) { l12 & l1 }
    assertResult(l2) { l2 & l12 }
    assertResult(ZBottom()) { l12 & ZNil() }
    assertResult(ZNil()) { l12Nil & ZNil() }
    assertResult(l12) { l12 & l12Nil }
    assertResult(ZBottom()) { l1 & l1b }
    assertResult(l1) { l13 & l1 }
    assertResult(l3) { l13 & l3 }
    assertResult(ZBottom()) { l13 & ZListLattice(List(lift(1), lift(2))) }
    assertResult(ZCons(lift(1, 4), ZCons(lift(2), ZMaybeNil(lift(3), ZNil())))) { l12 & l4 }
    assertResult(ZCons(lift(1, 4), ZCons(lift(5), ZNil()))) { l12 & l5 }
    assertResult(ZMaybeNil(lift(1), ZUnknownLength(lift(1)))) {
      ZMaybeNil(lift(1), ZUnknownLength(lift(1, 2))) & ZUnknownLength(lift(1))
    }
    assertResult(ZNil()) { ZMaybeNil(lift(1), ZUnknownLength(lift(1, 2))) & ZUnknownLength(lift(2)) }
    assertResult(ZBottom()) { ZCons(lift(1), ZUnknownLength(lift(1, 2))) & ZUnknownLength(lift(2)) }
    assertResult(ZMaybeNil(lift(2), ZNil())) { ZMaybeNil(lift(1, 2), ZUnknownLength(lift(1))) & ZUnknownLength(lift(2)) }
    assertResult(ZCons(lift(2), ZNil())) { ZCons(lift(1, 2), ZUnknownLength(lift(1))) & ZUnknownLength(lift(2)) }
  }

  test("Concat") {
    val l1 = ZListLattice(List(lift(1), lift(2), lift(3)))
    val l2 = ZListLattice(List(lift(1), lift(2)))
    val l3 = ZListLattice(List(lift(1)))

    val l12 = l1 | l2
    val l13 = l1 | l3

    assertResult(ZCons(lift(1), ZCons(lift(2), ZCons(lift(1), ZCons(lift(2), ZNil()))))) {
      l2 ++ l2
    }

    assertResult(ZCons(lift(1), ZCons(lift(2), ZCons(lift(3, 1), ZCons(lift(1, 2), ZMaybeNil(lift(2), ZNil())))))) {
      l12 ++ l2
    }

    assertResult(ZCons(lift(1), ZCons(lift(2), ZCons(lift(3, 1), ZCons(lift(1, 2), ZMaybeNil(lift(2, 3), ZMaybeNil(lift(3), ZNil()))))))) {
      l12 ++ l12
    }

    assertResult(ZCons(lift(1), ZCons(lift(1, 2), ZMaybeNil(lift(3), ZCons(lift(1), ZNil()))))) {
      l13 ++ l3
    }

    assertResult(ZCons(lift(1),ZCons(lift(2),ZTop()))) { l12 ++ ZTop() }

    assertResult(ZTop()) { ZTop() ++ l12 }
    assertResult(ZBottom()) { l12 ++ ZBottom() }
    assertResult(ZBottom()) { ZBottom() ++ l12 }
    assertResult(ZBottom()) { ZTop() ++ ZBottom() }
    assertResult(ZBottom()) { ZBottom() ++ ZTop() }

    assertResult(ZCons(Some(Set(1, 2)), ZTop())) { // result length is at least 1
      ZMaybeNil(lift(1), ZTop()) ++ ZCons(lift(2), ZTop())
    }
    assertResult(ZMaybeNil(Some(Set(1, 2)), ZTop())) {
      ZMaybeNil(lift(1), ZTop()) ++ ZMaybeNil(lift(2), ZTop())
    }
    assertResult(ZCons(lift(1), ZCons(lift(2, 4), ZUnknownLength(lift(3, 4, 5))))) {
      ZCons(lift(1), ZMaybeNil(lift(2), ZCons(lift(3), ZNil()))) ++ ZCons(lift(4), ZUnknownLength(lift(5)))
    }
    assertResult(ZCons(lift(1), ZUnknownLength(lift(2,3)))) {
      ZCons(lift(1), ZUnknownLength(lift(2))) ++ ZCons(lift(3), ZNil())
    }
    assertResult(ZUnknownLength(lift(1, 2, 3))) {
      ZUnknownLength(lift(1)) ++ l1
    }
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
    assertResult(true) { l1 <= ZUnknownLength(lift(1,2,3)) }
    assertResult(false) { ZUnknownLength(lift(1,2,3)) <= l1 }
    assertResult(true) { ZUnknownLength[L](None) <= ZTop() } // these are equal
    assertResult(true) { ZTop() <= ZUnknownLength[L](None) }
    assertResult(false) { l1 <= ZUnknownLength(lift(1,2)) } // these are incomparable
    assertResult(false) { ZUnknownLength(lift(1,2)) <= l1 }
  }

  test("Map") {
    val l1 = ZListLattice(List(lift(1), lift(2), lift(3)))
    val l2 = ZListLattice(List(lift(1), lift(2)))
    val l12 = l1 | l2

    assertResult(ZCons(lift(11),ZCons(lift(12),ZMaybeNil(lift(13),ZNil())))) { l12.map(_.map(_.map(_ + 10))) }
  }
}