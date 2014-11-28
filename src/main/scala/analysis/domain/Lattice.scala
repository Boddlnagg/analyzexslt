package analysis.domain

trait Lattice[A] {
  def top: A
  def bottom: A
  def join(left: A, right: A): A
  def meet(left: A, right: A): A
  def lessThanOrEqual(left: A, right: A): Boolean

  def joinAll(seq: Traversable[A]) = seq.fold(bottom)(join)
  def meetAll(seq: Traversable[A]) = seq.fold(top)(meet)
}

object Lattice {
  implicit def createFromOptionalSet[T]: Lattice[Option[Set[T]]] = new Lattice[Option[Set[T]]] {
    override def top: Option[Set[T]] = None
    override def bottom: Option[Set[T]] = Some(Set())
    override def join(left: Option[Set[T]], right: Option[Set[T]]): Option[Set[T]] = (left, right) match {
      case (None, _) | (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1 | s2)
    }
    override def meet(left: Option[Set[T]], right: Option[Set[T]]): Option[Set[T]] = (left, right) match {
      case (None, _) => right
      case (_, None) => left
      case (Some(s1), Some(s2)) => Some(s1 & s2)
    }
    override def lessThanOrEqual(left: Option[Set[T]], right: Option[Set[T]]): Boolean = (left, right) match {
      case (_, None) => true
      case (None, _) => false
      case (Some(s1), Some(s2)) => s1.subsetOf(s2)
    }
  }

  implicit object BooleanSetLattice extends Lattice[Set[Boolean]] {
    override def top: Set[Boolean] = Set(true, false)
    override def bottom: Set[Boolean] = Set()
    override def join(left: Set[Boolean], right: Set[Boolean]): Set[Boolean] = left | right
    override def meet(left: Set[Boolean], right: Set[Boolean]): Set[Boolean] = left & right
    override def lessThanOrEqual(left: Set[Boolean], right: Set[Boolean]): Boolean = left.subsetOf(right)
  }
}


