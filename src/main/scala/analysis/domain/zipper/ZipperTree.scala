package analysis.domain.zipper

abstract class NodeDescriptor
case object RootNode extends NodeDescriptor
case class ElementNode(name: String) extends NodeDescriptor
case class AttributeNode(name: String, value: String) extends NodeDescriptor
case class TextNode(value: String) extends NodeDescriptor
case class CommentNode(value: String) extends NodeDescriptor

case class ZipperTree(desc: NodeDescriptor, attributes: List[ZipperTree], children: List[ZipperTree]) {
  override def toString = desc match {
    case RootNode =>
      assert(children.size == 1)
      f"[${children.head}]" // wrap root node in `[]` to distinguish from element node
    case ElementNode(name) =>
      val attr = if (attributes.isEmpty) "" else " " + attributes.map(_.toString).mkString(" ")
      if (children.isEmpty) {
        f"<$name$attr/>"
      } else {
        val child = children.map(_.toString).mkString("")
        f"<$name$attr>$child</$name>"
      }
    case AttributeNode(name, value) => name + "=" + "\"" + value + "\""
    case TextNode(value) => value
    case CommentNode(value) => f"<!--$value-->"
  }
}

//case class ZipperTree(s: Set[(NodeDescriptor, List[ZipperTree])]) TODO: (infinite) sets

abstract class ZipperPath {
  def getDescriptor: NodeDescriptor
}
case object TopPath extends ZipperPath {
  def getDescriptor = RootNode
}

// left children as well as all attributes (because they are always to the left) are in reverse order
case class ElementNodePath(desc: NodeDescriptor, attributes: List[ZipperTree], left: List[ZipperTree], path: ZipperPath, right: List[ZipperTree]) extends ZipperPath {
  def getDescriptor = desc
}
// left attributes are in reverse order
case class AttributeNodePath(desc: AttributeNode, left: List[ZipperTree], path: ZipperPath, right: List[ZipperTree], children: List[ZipperTree]) extends ZipperPath {
  def getDescriptor = desc
}

case class ZipperLoc(subtree: ZipperTree, path: ZipperPath) {
  def goLeft = this.path match {
    case TopPath => throw new UnsupportedOperationException("Cannot go left on top")
    case ElementNodePath(_, Nil, Nil, _, _) => throw new UnsupportedOperationException("No sibling to the left")
    case ElementNodePath(_, a::attributes, Nil, up, right) => ZipperLoc(a, AttributeNodePath(a.desc.asInstanceOf[AttributeNode], attributes, up, Nil, this.subtree::right ))
    case ElementNodePath(_, attr, l::left, up, right) => ZipperLoc(l, ElementNodePath(l.desc, attr, left, up, this.subtree::right)) // keep attributes
    case AttributeNodePath(_, Nil, _, _, _) => throw new UnsupportedOperationException("No sibling to the left")
    case AttributeNodePath(_, l::left, up, right, children) => ZipperLoc(l, AttributeNodePath(l.desc.asInstanceOf[AttributeNode], left, up, this.subtree::right, children))
  }

  def goRight = this.path match {
    case TopPath => throw new UnsupportedOperationException("Cannot go right on top")
    case ElementNodePath(_, _, _, _, Nil) => throw new UnsupportedOperationException("No sibling to the right")
    case ElementNodePath(_, attr, left, up, r :: right) => ZipperLoc(r, ElementNodePath(r.desc, attr, this.subtree::left, up, right))
    case AttributeNodePath(_, _, _, Nil, Nil) => throw new UnsupportedOperationException("No sibling to the right")
    case AttributeNodePath(_, left, up, Nil, c :: children) => ZipperLoc(c, ElementNodePath(c.desc, this.subtree::left, Nil, up, children)) // no attributes left -> switch to children
    case AttributeNodePath(_, left, up, r :: right, children) => ZipperLoc(r, AttributeNodePath(r.asInstanceOf[AttributeNode], this.subtree::left, up, right, children))
  }

  def goUp = this.path match {
    case TopPath => throw new UnsupportedOperationException("Already on top, can't go up")
    case ElementNodePath(_, attributes, left, up, right) => ZipperLoc(ZipperTree(up.getDescriptor, attributes.reverse, left.reverse ++ (this.subtree :: right)), up)
    case AttributeNodePath(_, left, up, right, children) => ZipperLoc(ZipperTree(up.getDescriptor, left.reverse ++ (this.subtree::right), children), up)
  }

  def goDown = this.subtree match {
    case ZipperTree(_, Nil, Nil) => throw new UnsupportedOperationException("No attributes or children, can't go down")
    case ZipperTree(desc, Nil, first :: children) => ZipperLoc(first, ElementNodePath(first.desc, Nil, Nil, this.path, children))
    case ZipperTree(desc, first :: attributes, children) => ZipperLoc(first, AttributeNodePath(first.desc.asInstanceOf[AttributeNode], Nil, this.path, attributes, children))
  }
}
