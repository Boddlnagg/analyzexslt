import scala.xml._
import scala.collection.mutable

/** Abstract base class for XML nodes. The XML data model is defined in the XPath spec section 5.
  * NOTE: Processing instructions and namespaces are not implemented.
  */
abstract class XMLNode extends Ordered[XMLNode] {
  var parent: XMLNode

  /** Returns a list of all ancestors of this node */
  def ancestors : List[XMLNode] = this match {
    case XMLRoot(_) => Nil // root does not have ancestors
    case _ => parent :: parent.ancestors
  }

  /** Returns a list of all descendants of this node */
  def descendants : List[XMLNode] = this match {
    case XMLRoot(elem) => elem :: elem.descendants
    case XMLElement(_, _, children, _) => children.flatMap { ch => ch :: ch.descendants}.toList
    case XMLAttribute(_, _, _) => Nil
    case XMLTextNode(_, _) => Nil
  }

  /** Returns the root node (parent of parent, and so on, until XMLRoot is reached) */
  def root: XMLRoot = parent.root // works for all nodes except XMLRoot, where it is therefore overridden

  /** Compares two nodes regarding their position in the document order */
  def compare(that: XMLNode) = this.root.nodesInOrder.indexWhere(_ eq this) compare that.root.nodesInOrder.indexWhere(_ eq that)

  /** Returns the string-value of this node */
  def stringValue: String

  /** Returns a deep clone of this node */
  def copy: XMLNode
}

/** Root node as defined in XPath spec section 5.1.
  * NOTE: comments in the root node are not supported, processing instructions are generally not implemented
  */
case class XMLRoot(elem: XMLElement) extends XMLNode {
  elem.parent = this

  var parent : XMLNode = null // the root node is the only node that never has a parent

  override def toString = elem.toString

  override def equals(o: Any) = o match {
    // override equals because we need to ignore the parent to prevent endless recursion
    case that: XMLRoot => that.elem == this.elem
    case _ => false
  }

  /** Returns a list of all nodes (self and descendants) in document order */
  def nodesInOrder: List[XMLNode] = {
    XMLNode.nodesInOrder(this)
  }

  override def root = this

  override def stringValue = elem.stringValue

  override def copy = XMLRoot(elem.copy)
}

/** Element node as defined in XPath spec section 5.2. */
case class XMLElement(name: String,
                      var attributes: mutable.MutableList[XMLAttribute],
                      var children: mutable.MutableList[XMLNode],
                      var parent: XMLNode) extends XMLNode {

  /** Appends a new child node to this element (must not be an attribute node) */
  def appendChild(child: XMLNode): Unit = {
    assert(!child.isInstanceOf[XMLAttribute], "Children must not be attribute nodes.")
    assert(!child.isInstanceOf[XMLRoot], "Children must not be root nodes.")
    if (child.isInstanceOf[XMLTextNode] && !children.isEmpty && children.last.isInstanceOf[XMLTextNode]) {
      //merge text nodes with adjacent text nodes (at least xsl:value-of requires this (see XSLT spec section 7.6.1))
      val previousText = children.last.asInstanceOf[XMLTextNode].value
      val newText = child.asInstanceOf[XMLTextNode].value
      children.update(children.size - 1, XMLTextNode(previousText + newText, this))
    } else {
      children += child
      child.parent = this
    }
  }

  /** Adds an attribute to this element */
  def addAttribute(attribute: XMLAttribute) = {
    attributes += attribute
    attribute.parent = this
  }

  override def toString = {
    val attr = if (attributes.isEmpty) "" else " " + attributes.map(_.toString).mkString(" ")
    if (children.isEmpty) {
      f"<$name$attr/>"
    } else {
      val child = children.map(_.toString).mkString("")
      f"<$name$attr>$child</$name>"
    }
  }

  override def equals(o: Any) = o match {
    // override equals because we need to ignore the parent to prevent endless recursion
    case that: XMLElement => that.name == this.name && that.attributes == this.attributes && that.children == this.children
    case _ => false
  }

  override def stringValue = children.map(_.stringValue).mkString("")

  override def copy = XMLElement(name, attributes.map(a => a.copy), children.map(c => c.copy))
}

/** Attribute node as defined in XPath spec section 5.3. */
case class XMLAttribute(name: String, value: String, var parent: XMLNode = null) extends XMLNode {
  override def toString = {
    name + "=" + "\"" + value + "\""
  }

  override def equals(o: Any) = o match {
    // override equals because we need to ignore the parent to prevent endless recursion
    case that: XMLAttribute => that.name == this.name && that.value == this.value
    case _ => false
  }

  override def stringValue = value // NOTE: attribute-value normalization is required by the spec, but not implemented

  override def copy = XMLAttribute(name, value)
}
/** Comment node as defined in XPath spec section 5.6. */
case class XMLComment(value: String, var parent: XMLNode = null) extends XMLNode {
  override def toString = {
    f"<!--$value-->"
  }

  override def equals(o: Any) = o match {
    // override equals because we need to ignore the parent to prevent endless recursion
    case that: XMLComment => that.value == this.value
    case _ => false
  }

  override def stringValue = value

  override def copy = XMLComment(value)
}
/** Text node as defined in XPath spec section 5.7. */
case class XMLTextNode(value: String, var parent: XMLNode = null) extends XMLNode {
  override def toString = {
    value
  }

  override def equals(o: Any) = o match {
    // override equals because we need to ignore the parent to prevent endless recursion
    case that: XMLTextNode => that.value == this.value
    case _ => false
  }

  override def stringValue = value

  override def copy = XMLTextNode(value)
}

/** Factory for [[XMLElement]] instances */
object XMLElement {
  /** Creates an element from a [[scala.xml.Elem]] node */
  def apply(elem: Elem) : XMLElement = {
    XMLNode(elem).asInstanceOf[XMLElement]
  }

  /** Creates an element from a name as well as optional lists of attributes and children. */
  def apply(name: String, attributes: Seq[XMLAttribute] = Nil, children: Seq[XMLNode] = Nil): XMLElement = {
    val result = new XMLElement(name, mutable.MutableList(), mutable.MutableList(), null)
    attributes.foreach(attr => result.addAttribute(attr))
    children.foreach(child => result.appendChild(child))
    result
  }
}

/** Factory for [[XMLRoot]] instances */
object XMLRoot {
  /** Creates a root node from a [[scala.xml.Elem]] node */
  def apply(elem: Elem): XMLRoot = {
    XMLRoot(XMLElement(elem))
  }
}

/** Factory for [[XMLNode]] instances */
object XMLNode {
  /** Creates a node from a [[scala.xml.Node]] */
  def apply(node: Node) : XMLNode = {
    node match {
      case elem: Elem =>
        if (elem.namespace != null && elem.namespace != "") throw new NotImplementedError("Prefixed names are not implemented")
        XMLElement(elem.label,
          elem.attributes.asAttrMap.map { case (name, value) => XMLAttribute(name, value) }.toList,
          elem.child.map(n => apply(n)).toList
        )
      case text: Text => XMLTextNode(text.data)
      case comment: Comment => XMLComment(comment.commentText)
      case _ => throw new NotImplementedError(f"Unsupported XML node: ${node.getClass} ($node)")
    }
  }

  /** Returns a list of nodes in document order, starting from a given node */
  def nodesInOrder(start: XMLNode): List[XMLNode] = {
    start match {
      case XMLRoot(elem) => List(start) ++ nodesInOrder(elem)
      case XMLElement(_, attr, children, _) => List(start) ++ attr ++ children.flatMap(ch => nodesInOrder(ch))
      case XMLTextNode(_, _) => List(start)
      case XMLComment(_, _) => List(start)
    }
  }
}
