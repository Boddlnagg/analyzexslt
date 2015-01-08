package xml

/** Root node as defined in XPath spec section 5.1.
  * NOTE: comments in the root node are not supported, processing instructions are generally not implemented
  */
case class XMLRoot(inner: XMLElement) extends XMLNode {
  inner.parent = this

  var parent: XMLNode = null // the root node is the only node that never has a parent

  override def toString = f"[${inner.toString}]" // wrap content in "[]" so we can distinguish the content from the root-node

  override def equals(o: Any) = o match {
    // override equals because we need to ignore the parent to prevent endless recursion
    case that: XMLRoot => that.inner == this.inner
    case _ => false
  }

  override def hashCode = inner.hashCode * 41 * 41

  /** Returns a list of all nodes (self and descendants) in document order */
  def nodesInOrder: List[XMLNode] = {
    XMLNode.nodesInOrder(this)
  }

  override def root = this

  override def stringValue = inner.stringValue

  override def copy = XMLRoot(inner.copy)
}