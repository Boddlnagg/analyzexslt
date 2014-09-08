package xml

import scala.collection.mutable

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

/** Factory for [[XMLElement]] instances */
object XMLElement {
  /** Creates an element from a name as well as optional lists of attributes and children. */
  def apply(name: String, attributes: Seq[XMLAttribute] = Nil, children: Seq[XMLNode] = Nil): XMLElement = {
    val result = new XMLElement(name, mutable.MutableList(), mutable.MutableList(), null)
    attributes.foreach(attr => result.addAttribute(attr))
    children.foreach(child => result.appendChild(child))
    result
  }
}