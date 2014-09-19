package analysis.domain

import xpath._

object PowersetXPathDomain {
  type T = Option[Set[XPathValue]] // None represents the infinite set, Some represents finite sets

  object D extends XPathDomain[T] {
    override def top: T = Some(Set())
    override def bottom: T = None

    override def lift(v: XPathValue): T = Some(Set(v))

    override def join(n1: T, n2: T): T = (n1, n2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.union(s2))
    }

    override def meet(n1: T, n2: T): T = (n1, n2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.intersect(s2))
    }
  }
}
