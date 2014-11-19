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

trait ZipperPath {
  def getDescriptor: NodeDescriptor
  def shrink: LastZipperPath
}
case object TopPath extends ZipperPath with LastZipperPath {
  def getDescriptor = RootNode
  def extend(desc: NodeDescriptor) = this
  def shrink = this
}

// left children are in reverse order
case class NodePath(desc: NodeDescriptor, left: List[ZipperTree], parent: ZipperPath, right: List[ZipperTree]) extends ZipperPath {
  def getDescriptor = desc
  def shrink = LastNodePath(left, parent, right)
}

trait LastZipperPath {
  def extend(desc: NodeDescriptor): ZipperPath
}

// left children are in reverse order
case class LastNodePath(left: List[ZipperTree], parent: ZipperPath, right: List[ZipperTree]) extends LastZipperPath {
  def extend(desc: NodeDescriptor) = NodePath(desc, left, parent, right)
}

case class ZipperLoc(subtree: ZipperTree, path: LastZipperPath) {
  def goLeft = this.path match {
    case TopPath => throw new UnsupportedOperationException("Cannot go left on top")
    case LastNodePath(Nil, _, _) => throw new UnsupportedOperationException("No sibling to the left")
    case LastNodePath(l::left, up, right) => ZipperLoc(l, LastNodePath(Nil, up, this.subtree::right))
  }

  def goRight = this.path match {
    case TopPath => throw new UnsupportedOperationException("Cannot go right on top")
    case LastNodePath(_, _, Nil) => throw new UnsupportedOperationException("No sibling to the right")
    case LastNodePath(left, up, r :: right) => ZipperLoc(r, LastNodePath(this.subtree::left, up, right))
  }

  def goUp = this.path match {
    case TopPath => throw new UnsupportedOperationException("Already on top, can't go up")
    case LastNodePath(left, up, right) => ZipperLoc(ZipperTree(up.getDescriptor, left.reverse ++ (this.subtree :: right)), up.shrink)
  }

  def goDown = this.subtree match {
    case ZipperTree(_, Nil) => throw new UnsupportedOperationException("No attributes or children, can't go down")
    case ZipperTree(desc, first :: children) => ZipperLoc(first, LastNodePath(Nil, this.path.extend(desc), children))
  }
}
