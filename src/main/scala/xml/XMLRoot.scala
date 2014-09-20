package xml

/** Root node as defined in XPath spec section 5.1.
  * NOTE: comments in the root node are not supported, processing instructions are generally not implemented
  */
case class XMLRoot(elem: XMLElement) extends XMLNode {
  elem.parent = this

  var parent: XMLNode = null // the root node is the only node that never has a parent

  override def toString = elem.toString

  override def equals(o: Any) = o match {
    // override equals because we need to ignore the parent to prevent endless recursion
    case that: XMLRoot => that.elem == this.elem
    case _ => false
  }

  override def hashCode = elem.hashCode * 41 * 41

  /** Returns a list of all nodes (self and descendants) in document order */
  def nodesInOrder: List[XMLNode] = {
    XMLNode.nodesInOrder(this)
  }

  override def root = this

  override def stringValue = elem.stringValue

  override def copy = XMLRoot(elem.copy)
}