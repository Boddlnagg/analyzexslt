package analysis.domain.zipper.set

abstract class NodeDescriptor
case object RootNode extends NodeDescriptor
case class ElementNode(name: String) extends NodeDescriptor
case class AttributeNode(name: String, value: String) extends NodeDescriptor
case class TextNode(value: String) extends NodeDescriptor
case class CommentNode(value: String) extends NodeDescriptor

abstract class ZipperTree {
  def getDescriptor: Option[Set[NodeDescriptor]]
}

case object InfZipperTree extends ZipperTree {
  def getDescriptor = None
}

case class FinZipperTree(nodes: Set[ZipperTreeNode]) extends ZipperTree {
  def getDescriptor = Some(nodes.map(n => n.desc))
}

case class ZipperTreeNode(desc: NodeDescriptor, children: List[ZipperTree]) {
  /*override def toString = desc match {
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
  }*/
}

//case class ZipperTree(s: Set[(NodeDescriptor, List[ZipperTree])]) TODO: (infinite) sets

abstract class ZipperPath {
  def getDescriptor: Option[Set[NodeDescriptor]]
}
case object TopPath extends ZipperPath {
  def getDescriptor = Some(Set(RootNode))
}

// left children are in reverse order
case class NodePath(desc: Option[Set[NodeDescriptor]], left: List[ZipperTree], parent: ZipperPath, right: List[ZipperTree]) extends ZipperPath {
  def getDescriptor = desc
}

case class InfNodePath(parent: ZipperPath) extends ZipperPath {
  def getDescriptor = None
}

case class ZipperLoc(subtree: ZipperTree, path: ZipperPath) {
  /*def goLeft = this.path match {
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
  }*/

  def goDown = this.subtree match {
    case InfZipperTree => ZipperLoc(InfZipperTree, InfNodePath(this.path))
    case FinZipperTree(s) =>
      val (subtrees, paths) = s.collect {
        case ZipperTreeNode(desc, first :: children) => (first, NodePath(first.getDescriptor, Nil, this.path, children))
      }.unzip
      ZipperLoc(flattenZipperTreeSet(subtrees), ???)
    // case ZipperTree(desc, first :: children) => ZipperLoc(first, NodePath(first.desc, Nil, this.path, children))
  }

  private def flattenZipperTreeSet(set: Set[ZipperTree]): ZipperTree = {
    FinZipperTree(set.map {
      case InfZipperTree => return InfZipperTree
      case FinZipperTree(s) => s
    }.flatten)
  }
}
