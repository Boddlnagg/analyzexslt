package analysis

/** Base class for ordering results in lattice domain value comparisons */
abstract class LatticeOrdering

case object Less extends LatticeOrdering
case object Greater extends LatticeOrdering
case object Equal extends LatticeOrdering
case object Incomparable extends LatticeOrdering
