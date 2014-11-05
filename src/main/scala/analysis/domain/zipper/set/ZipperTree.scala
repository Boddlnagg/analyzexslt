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

abstract class ZipperPath {
  def getDescriptor: Option[Set[NodeDescriptor]]
  def getParent: Option[Set[ZipperPath]]
}

case object InfZipperPath extends ZipperPath {
  override def getDescriptor = None
  override def getParent = None
}

case class FinParentZipperPath(parent: Set[ZipperPath]) extends ZipperPath {
  override def getDescriptor = None
  override def getParent = Some(parent)
}

case class FinZipperPath(set: Set[PathSegment]) extends ZipperPath {
  override def getDescriptor: Option[Set[NodeDescriptor]] = Some(set.map(seg => seg.getDescriptor match {
    case None => return None
    case Some(desc) => desc
  }))

  override def getParent = Some(set.map {
    case TopPathSegment => Nil
    case NodePathSegment(_, _, parent, _) => List(parent)
  }.flatten.toSet)
}


abstract class PathSegment {
  def getDescriptor: Option[NodeDescriptor]
}
case object TopPathSegment extends PathSegment {
  override def getDescriptor = Some(RootNode)
}
case class NodePathSegment(desc: Option[NodeDescriptor], left: List[ZipperTree], parent: ZipperPath, right: List[ZipperTree]) extends PathSegment {
  override def getDescriptor = desc
}

/*abstract class ZipperPath {
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
}*/

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
    case InfZipperTree => ZipperLoc(InfZipperTree, FinParentZipperPath(Set(this.path)))
    case FinZipperTree(s) =>
      val (subtrees, paths) = s.collect {
        case ZipperTreeNode(desc, first :: children) => (first, makePath(first.getDescriptor, Nil, this.path, children))
      }.unzip
      ZipperLoc(flattenZipperTreeSet(subtrees), flattenFinZipperPathSet(paths))
  }

  private def flattenZipperTreeSet(set: Set[ZipperTree]): ZipperTree =
    FinZipperTree(set.map {
      case InfZipperTree => return InfZipperTree
      case FinZipperTree(s) => s
    }.flatten)

  private def flattenFinZipperPathSet(set: Set[FinZipperPath]): FinZipperPath =
    FinZipperPath(set.map(p => p.set).flatten)

  private def makePath(desc: Option[Set[NodeDescriptor]], left: List[ZipperTree], parent: ZipperPath, right: List[ZipperTree]): FinZipperPath = {
    desc match {
      case None => FinZipperPath(Set(NodePathSegment(None, left, parent, right)))
      case Some(s) => FinZipperPath(s.map(desc => NodePathSegment(Some(desc), left, parent, right)))
    }
  }
}
