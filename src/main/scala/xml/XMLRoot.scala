package xml

/** Root node as defined in XPath spec section 5.1.
  * NOTE: comments in the root node are not supported, processing instructions are generally not implemented
  */
case class XMLRoot(children: List[XMLNode]) extends XMLNode {

  children.foreach { ch => ch.parent = this }

  var parent: XMLNode = null // the root node is the only node that never has a parent

  override def toString = f"[${children.map(_.toString).mkString}]" // wrap content in "[]" so we can distinguish the content from the root-node

  /** Returns a list of all nodes (self and descendants) in document order */
  def nodesInOrder: List[XMLNode] = {
    XMLNode.nodesInOrder(this)
  }

  override def root = this

  override def stringValue = children.collect {
    case XMLTextNode(text, _) => text
    case e: XMLElement => e.stringValue
  }.mkString

  override def copy = XMLRoot(children.map(_.copy))
}