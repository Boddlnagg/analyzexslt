package xml

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

  override def hashCode = value.hashCode * 41 * 41

  override def stringValue = value

  override def copy = XMLTextNode(value)
}