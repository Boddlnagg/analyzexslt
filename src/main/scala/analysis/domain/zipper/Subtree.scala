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

  def prettyPrint: String = {
    val b = new StringBuilder

    def formatSet(set: List[String]): String = set match {
      case Nil => ""
      case single :: Nil => single
      case more => "{" + more.mkString(",") + "}"
    }

    def appendSubtreeIndented(tree: Subtree, indentLevel: Int): Unit = {
      val desc = tree.desc.toList
      val elems = if (tree.desc.contains(AnyElement)) None else Some(desc.collect { case Element(name) => name })
      val text = if (tree.desc.contains(AnyText)) None else Some(desc.collect { case Text(value) => value })
      val comments = if (tree.desc.contains(AnyComment)) None else Some(desc.collect { case Comment(value) => value })

      val elemNames: List[String] = elems.getOrElse(List("*"))
      val textString: List[String] = text match {
        case None => List("![ANY-TEXT]")
        case Some(set) => set.map(value => "![TEXT[" + value + "]]")
      }
      val commentString: List[String] = comments match {
        case None => List("![ANY-COMMENT]")
        case Some(set) => set.map(value => "![COMMENT[" + value + "]]")
      }

      b.append("<")
      b.append(formatSet(elemNames ++ textString ++ commentString))
      assert (tree.attributes != ZBottom() && tree.children != ZBottom()) // attributes or children should never be bottom (or else the whole tree should be bottom)
      appendAttributes(tree.attributes)

      if (tree.children == ZNil()) {
        if (elemNames.nonEmpty) b.append(" />")
        else b.append(">")
      } else {
        b.append(">")
        appendChildrenIndented(tree.children, indentLevel)
        b.append("</")
        if (elemNames.nonEmpty) b.append(formatSet(elemNames))
        b.append(">")
      }
    }

    def appendAttributes(attr: ZList[Set[NodeDescriptor]]): Unit = attr match {
      case ZNil() => ()
      case ZTop() => b.append(" any-attributes")
      case ZUnknownLength(elem) =>
        if (elem.contains(AnyAttribute)) b.append(" any-attributes")
        else b.append(" ").append(elem.map(formatAttribute).mkString(" "))
    }

    def formatAttribute(desc: NodeDescriptor): String = desc match {
      case Attribute(name, value) => name + "=\"" + value + "\"?"
      case NamedAttribute(name) => name + "=*?"
    }

    def appendChildrenIndented(children: ZList[Subtree], indentLevel: Int): Unit = children match {
      case ZBottom() => b.append("BOTTOM")
      case ZTop() => b.append("TOP")
      case _ =>
        b.append("\n")
        b.append("\t" * (indentLevel + 1))
        appendElementIndented(children.asInstanceOf[ZListElement[Subtree]], indentLevel + 1)
        b.append("\n")
        b.append("\t" * indentLevel)
    }

    def appendElementIndented(elem: ZListElement[Subtree], indentLevel: Int): Unit = elem match {
      case ZTop() => b.append("TOP")
      case ZUnknownLength(elems) =>
        appendSubtreeIndented(elems, indentLevel)
        b.append("\n")
        b.append("\t" * indentLevel)
        b.append("...")
      case ZCons(first, ZNil()) => appendSubtreeIndented(first, indentLevel)
      case ZCons(first, rest) =>
        appendSubtreeIndented(first, indentLevel)
        b.append("\n")
        b.append("\t" * indentLevel)
        appendElementIndented(rest, indentLevel)
      case ZMaybeNil(first, ZNil()) =>
        b.append("NIL + ")
        appendSubtreeIndented(first, indentLevel)
      case ZMaybeNil(first, rest) =>
        b.append("NIL + ")
        appendSubtreeIndented(first, indentLevel)
        b.append("\n")
        b.append("\t" * indentLevel)
        appendElementIndented(rest, indentLevel)
    }

    if (this.desc == Set()) { // empty set -> BOTTOM
      b.append("BOTTOM")
    } else {
      // assert that top level subtree is root node with no attributes and a single child
      assert(this.desc == Set(Root) && this.attributes == ZNil())
      // ... then only print children of root node
      appendElementIndented(this.children.asInstanceOf[ZListElement[Subtree]], 0)
    }
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

