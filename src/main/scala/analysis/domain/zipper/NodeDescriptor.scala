package analysis.domain.zipper

import analysis.domain.Lattice

abstract class NodeDescriptor
case object Root extends NodeDescriptor
case class Element(name: String) extends NodeDescriptor
case object AnyElement extends NodeDescriptor
case class Attribute(name: String, value: String) extends NodeDescriptor
case class NamedAttribute(name: String) extends NodeDescriptor
case object AnyAttribute extends NodeDescriptor
case class Text(value: String) extends NodeDescriptor {
  override def toString = f"Text(${value.replace("\n", "\\n")})"
}
case object AnyText extends NodeDescriptor
case class Comment(value: String) extends NodeDescriptor
case object AnyComment extends NodeDescriptor

object NodeDescriptor {
  implicit object NodeDescriptorLattice extends Lattice[Set[NodeDescriptor]] {
    def top = Set(
      Root,
      AnyElement,
      AnyAttribute,
      AnyComment,
      AnyText
    )

    def bottom = Set()

    def join(left: Set[NodeDescriptor], right: Set[NodeDescriptor]): Set[NodeDescriptor] = normalizeDescriptors(left | right)

    def meet(left: Set[NodeDescriptor], right: Set[NodeDescriptor]): Set[NodeDescriptor] = {
      val result = left.cross(right).flatMap {
        case (p1, p2) => meetSingle(p1, p2)
      }
      normalizeDescriptors(result.toSet)
    }

    def lessThanOrEqual(left: Set[NodeDescriptor], right: Set[NodeDescriptor]): Boolean = {
      left.forall(desc1 => right.exists(desc2 => lessThanOrEqualSingle(desc1, desc2)))
    }

    def normalizeDescriptors(set: Set[NodeDescriptor]): Set[NodeDescriptor] = set.toList.foldRight(List[NodeDescriptor]()) {
      case (next, acc) => if (acc.exists(e => lessThanOrEqualSingle(next, e)))
        acc
      else
        next :: acc.filter(e => !lessThanOrEqualSingle(e, next))
    }.toSet

    def lessThanOrEqualSingle(desc1: NodeDescriptor, desc2: NodeDescriptor): Boolean = {
      (desc1, desc2) match {
        case _ if desc1 == desc2 => true // equal
        case (Element(_), AnyElement) => true // less than
        case (Attribute(_, _), AnyAttribute) => true // less than
        case (Attribute(name1, _), NamedAttribute(name2)) if name1 == name2 => true // less than
        case (NamedAttribute(_), AnyAttribute) => true // less than
        case (Text(_), AnyText) => true // less than
        case (Comment(_), AnyComment) => true // less than
        case _ => false
      }
    }

    private def meetSingle(desc1: NodeDescriptor, desc2: NodeDescriptor): Option[NodeDescriptor] = {
      (desc1, desc2) match {
        case _ if desc1 == desc2 => Some(desc1)
        case (n@Element(_), AnyElement) => Some(n)
        case (AnyElement, n@Element(_)) => Some(n)
        case (n@Attribute(_, _), AnyAttribute) => Some(n)
        case (AnyAttribute, n@Attribute(_, _)) => Some(n)
        case (n@Attribute(name1, _), NamedAttribute(name2)) if name1 == name2 => Some(n)
        case (NamedAttribute(name2), n@Attribute(name1, _)) if name1 == name2 => Some(n)
        case (n@NamedAttribute(_), AnyAttribute) => Some(n)
        case (AnyAttribute, n@NamedAttribute(_)) => Some(n)
        case (n@Text(_), AnyText) => Some(n)
        case (AnyText, n@Text(_)) => Some(n)
        case (n@Comment(_), AnyComment) => Some(n)
        case (AnyComment, n@Comment(_)) => Some(n)
        case _ => None // represents BOTTOM here
      }
    }
  }
}