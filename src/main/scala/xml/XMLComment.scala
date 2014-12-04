package xml

// TODO: remove comments (or allow them to be created in the abstract domain)?
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

  override def hashCode = value.hashCode * 41

  override def stringValue = value

  override def copy = XMLComment(value)
}
