package analysis.domain.zipper

import analysis.domain.Lattice

case class Subtree(desc: Set[NodeDescriptor], attributes: ZList[Set[NodeDescriptor]], children: ZList[Subtree]) {
  override def toString: String = {
    val b = new StringBuilder

    def appendSubtreeIndented(tree: Subtree, indentLevel: Int): Unit = {
      b.append("Subtree(")
      b.append(tree.desc.toString())
      b.append(",")
      b.append(tree.attributes.toString)
      b.append(",")
      appendChildrenIndented(tree.children, indentLevel)
      b.append(")")
    }

    def appendChildrenIndented(children: ZList[Subtree], indentLevel: Int): Unit = children match {
      case ZBottom() => b.append("[BOTTOM]")
      case ZNil() => b.append("[]")
      case ZTop() => b.append("[TOP]")
      case _ =>
        b.append("[\n")
        b.append("\t" * (indentLevel + 1))
        appendElementIndented(children.asInstanceOf[ZListElement[Subtree]], indentLevel + 1)
        b.append("\n")
        b.append("\t" * indentLevel)
        b.append("]")
    }

    def appendElementIndented(elem: ZListElement[Subtree], indentLevel: Int): Unit = elem match {
      case ZTop() => b.append("TOP")
      case ZUnknownLength(elems) =>
        appendSubtreeIndented(elems, indentLevel)
        b.append("*")
      case ZCons(first, ZNil()) => appendSubtreeIndented(first, indentLevel)
      case ZCons(first, rest) =>
        appendSubtreeIndented(first, indentLevel)
        b.append(",\n")
        b.append("\t" * indentLevel)
        appendElementIndented(rest, indentLevel)
      case ZMaybeNil(first, ZNil()) =>
        b.append("NIL+")
        appendSubtreeIndented(first, indentLevel)
      case ZMaybeNil(first, rest) =>
        b.append("NIL+")
        appendSubtreeIndented(first, indentLevel)
        b.append(",\n")
        b.append("\t" * indentLevel)
        appendElementIndented(rest, indentLevel)
    }

    appendSubtreeIndented(this, 0)
    b.toString()
  }
}

object Subtree {
  private val latD = NodeDescriptor.NodeDescriptorLattice

  implicit object SubtreeLattice extends Lattice[Subtree] {
    override def top = Subtree(latD.top, ZUnknownLength(Set(AnyAttribute)), ZTop())
    override def bottom = Subtree(latD.bottom, ZBottom(), ZBottom())
    override def join(left: Subtree, right: Subtree): Subtree =
      Subtree(latD.join(left.desc, right.desc), left.attributes | right.attributes, left.children | right.children)
    override def meet(left: Subtree, right: Subtree): Subtree =
      Subtree(latD.meet(left.desc, right.desc), left.attributes & right.attributes, left.children & right.children)
    override def lessThanOrEqual(left: Subtree, right: Subtree): Boolean =
      latD.lessThanOrEqual(left.desc, right.desc) &&
        left.attributes <= right.attributes &&
        left.children <= right.children
  }
}

