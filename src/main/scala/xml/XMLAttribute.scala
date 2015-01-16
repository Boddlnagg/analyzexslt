package xml

/** Attribute node as defined in XPath spec section 5.3. */
case class XMLAttribute(name: String, value: String, var parent: XMLNode = null) extends XMLNode {
  if (name.isEmpty) throw new IllegalArgumentException("Cannot create XML attribute with empty name.")

  override def toString = {
    name + "=" + "\"" + value + "\""
  }

  override def equals(o: Any) = o match {
    case that: XMLAttribute =>
      if (this.root != null && (this.root eq that.root)) {
        // if both nodes belong to the same document (and not just a fragment), compare using reference equality
        this eq that
      } else {
        // otherwise we ignore the parent to prevent endless recursion
        that.name == this.name && that.value == this.value
      }
    case _ => false
  }

  override def hashCode = name.hashCode + value.hashCode * 41

  override def stringValue = value // NOTE: attribute-value normalization is required by the spec, but not implemented

  override def copy = XMLAttribute(name, value)
}
