import scala.xml._
import scala.collection.mutable.MutableList

abstract class XMLNode {
  var parent: XMLNode

  def ancestors : List[XMLNode] = {
    if (parent != null) parent :: parent.ancestors
    else Nil
  }
}

// XPath spec section 5.1 (comments in the root node are not supported, processing instructions are generally not implemented)
case class XMLRoot(elem: XMLElement) extends XMLNode {
  elem.parent = this

  var parent : XMLNode = null
  override def toString = elem.toString
}

// XPath spec section 5.2
case class XMLElement(name: String,
                      var attributes: MutableList[XMLAttribute],
                      var children: MutableList[XMLNode],
                      var parent: XMLNode) extends XMLNode {

  def appendChild(child: XMLNode) = {
    assert(!child.isInstanceOf[XMLAttribute], "Children must not be attribute nodes.")
    children += child
    child.parent = this
  }

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
}

// XPath spec section 5.3
case class XMLAttribute(name: String, value: String, var parent: XMLNode = null) extends XMLNode {
  override def toString = {
    name + "=" + "\"" + value + "\""
  }
}
// XPath spec section 5.6
case class XMLComment(value: String, var parent: XMLNode = null) extends XMLNode {
  override def toString = {
    f"<!--$value-->"
  }
}
// XPath spec section 5.7
case class XMLTextNode(value: String, var parent: XMLNode = null) extends XMLNode {
  override def toString = {
    value
  }
}
// processing instructions and namespaces are not implemented

object XMLElement {
  def apply(elem: Elem) : XMLElement = {
    XMLNode(elem).asInstanceOf[XMLElement]
  }

  def apply(name: String, attributes: Seq[XMLAttribute] = Nil, children: Seq[XMLNode] = Nil): XMLElement = {
    val result = new XMLElement(name, MutableList(), MutableList(), null)
    attributes.foreach(attr => result.addAttribute(attr))
    children.foreach(child => result.appendChild(child))
    result
  }
}

object XMLRoot {
  def apply(elem: Elem): XMLRoot = {
    XMLRoot(XMLElement(elem))
  }
}

object XMLNode {
  def apply(node: Node) : XMLNode = {
    node match {
      case elem: Elem =>
        assert(elem.namespace == null || elem.namespace == "", "Prefixed names are not supported")
        XMLElement(elem.label,
          elem.attributes.asAttrMap.map { case (name, value) => XMLAttribute(name, value) }.toList,
          elem.child.map(n => apply(n)).toList
        )
      case text: Text => XMLTextNode(text.data)
      case comment: Comment => XMLComment(comment.commentText)
      case _ => throw new UnsupportedOperationException(f"Unsupported XML node: ${node.getClass} ($node)")
    }
  }
}
