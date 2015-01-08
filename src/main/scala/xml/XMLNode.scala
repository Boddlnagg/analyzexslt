package xml

/** Abstract base class for XML nodes. The XML data model is defined in the XPath spec section 5.
  * NOTE: Processing instructions and namespaces are not implemented.
  */
abstract class XMLNode extends Ordered[XMLNode] {
  var parent: XMLNode

  /** Returns a list of all ancestors of this node */
  def ancestors: List[XMLNode] = this match {
    case XMLRoot(_) => Nil // root does not have ancestors
    case _ => parent :: parent.ancestors
  }

  /** Returns a list of all descendants of this node */
  def descendants: List[XMLNode] = this match {
    case XMLRoot(inner) => inner :: inner.descendants
    case XMLElement(_, _, children, _) => children.flatMap { ch => ch :: ch.descendants}.toList
    case XMLAttribute(_, _, _) => Nil
    case XMLTextNode(_, _) => Nil
  }

  /** Returns the root node (parent of parent, and so on, until XMLRoot is reached).
    * If this is null, the current node belongs to a tree fragment without an enclosing document.
    */
  def root: XMLRoot = if (parent != null) parent.root else null // works for all nodes except XMLRoot, where it is therefore overridden

  /** Compares two nodes regarding their position in the document order */
  def compare(that: XMLNode) = this.root.nodesInOrder.indexWhere(_ eq this) compare that.root.nodesInOrder.indexWhere(_ eq that)

  /** Returns the string-value of this node */
  def stringValue: String

  /** Returns a deep clone of this node */
  def copy: XMLNode
}

/** Helper functions for XML nodes. */
object XMLNode {
  /** Returns a list of nodes in document order, starting from a given node */
  def nodesInOrder(start: XMLNode): List[XMLNode] = {
    start match {
      case XMLRoot(inner) => List(start) ++ nodesInOrder(inner)
      case XMLElement(_, attr, children, _) => List(start) ++ attr ++ children.flatMap(ch => nodesInOrder(ch))
      case XMLTextNode(_, _) => List(start)
      case XMLComment(_, _) => List(start)
    }
  }

  /** Formats the path where a node can be reached (pseudo XPath syntax) */
  def formatPath(node: XMLNode): String = {
    def internal(n: XMLNode) = n match {
      case XMLRoot(_) => "/"
      case XMLElement(name, _, _, parent) => formatPath(parent) + "name/"
      case XMLTextNode(_, parent) => formatPath(parent) + "<text>"
      case XMLComment(_, parent) => formatPath(parent) + "<comment>"
      case XMLAttribute(name, _, parent) => formatPath(parent) + "@" + name
      case null => ""
    }

    val result = internal(node)
    // strip last "/" if any (and not just "/")
    if (result.length > 1 && result.endsWith("/")) result.substring(0, result.length - 1) else result
  }
}