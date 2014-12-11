package xml

/** Comment node as defined in XPath spec section 5.6. */
case class XMLComment(value: String, var parent: XMLNode = null) extends XMLNode {
  override def toString = {
    f"<!--$value-->"
  }

  override def equals(o: Any) = o match {

    case that: XMLComment =>
      if (this.root != null && (this.root eq that.root)) {
        // if both nodes belong to the same document (and not just a fragment), compare using reference equality
        this eq that
      } else {
        // otherwise we ignore the parent to prevent endless recursion
        that.value == this.value
      }
    case _ => false
  }

  override def hashCode = value.hashCode * 41

  override def stringValue = value

  override def copy = XMLComment(value)
}
