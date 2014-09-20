package xml

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

  override def hashCode = name.hashCode + value.hashCode * 41

  override def stringValue = value // NOTE: attribute-value normalization is required by the spec, but not implemented

  override def copy = XMLAttribute(name, value)
}
