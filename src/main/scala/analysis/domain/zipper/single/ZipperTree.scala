package analysis.domain.zipper.single

abstract class NodeDescriptor
case object RootNode extends NodeDescriptor
case class ElementNode(name: String) extends NodeDescriptor
case class AttributeNode(name: String, value: String) extends NodeDescriptor
case class TextNode(value: String) extends NodeDescriptor
case class CommentNode(value: String) extends NodeDescriptor

case class ZipperTree(desc: NodeDescriptor, children: List[ZipperTree]) {
  override def toString = desc match {
    case RootNode =>
      assert(children.size == 1)
      f"[${children.head}]" // wrap root node in `[]` to distinguish from element node
    case ElementNode(name) =>
      val (attributes, realChildren) = children.partition(c => c.desc.isInstanceOf[AttributeNode])
      val attr = if (attributes.isEmpty) "" else " " + attributes.map(_.toString).mkString(" ")
      if (realChildren.isEmpty) {
        f"<$name$attr/>"
      } else {
        val child = realChildren.map(_.toString).mkString("")
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

// left children are in reverse order
case class NodePath(desc: NodeDescriptor, left: List[ZipperTree], parent: ZipperPath, right: List[ZipperTree]) extends ZipperPath {
  def getDescriptor = desc
}

case class ZipperLoc(subtree: ZipperTree, path: ZipperPath) {
  def goLeft = this.path match {
    case TopPath => throw new UnsupportedOperationException("Cannot go left on top")
    case NodePath(_, Nil, _, _) => throw new UnsupportedOperationException("No sibling to the left")
    case NodePath(_, l::left, up, right) => ZipperLoc(l, NodePath(l.desc, Nil, up, this.subtree::right))
  }

  def goRight = this.path match {
    case TopPath => throw new UnsupportedOperationException("Cannot go right on top")
    case NodePath(_, _, _, Nil) => throw new UnsupportedOperationException("No sibling to the right")
    case NodePath(_, left, up, r :: right) => ZipperLoc(r, NodePath(r.desc, this.subtree::left, up, right))
  }

  def goUp = this.path match {
    case TopPath => throw new UnsupportedOperationException("Already on top, can't go up")
    case NodePath(_, left, up, right) => ZipperLoc(ZipperTree(up.getDescriptor, left.reverse ++ (this.subtree :: right)), up)
  }

  def goDown = this.subtree match {
    case ZipperTree(_, Nil) => throw new UnsupportedOperationException("No attributes or children, can't go down")
    case ZipperTree(_, first :: children) => ZipperLoc(first, NodePath(first.desc, Nil, this.path, children))
  }
}
