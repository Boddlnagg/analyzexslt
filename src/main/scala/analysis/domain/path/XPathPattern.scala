package analysis.domain.path

abstract class XPathPattern {
  def prev: Option[XPathPattern]
  def withPrev(pattern: XPathPattern): XPathPattern
  def stepString: String

  override def toString = prev match {
    case None => stepString
    case Some(Root) => "/" + stepString
    case Some(p) => p + "/" + stepString
  }
}

// TODO: use this class instead of Option with None?
/*case class AnyNode(prev: Option[XPathPattern]) extends XPathPattern*/

case object Root extends XPathPattern {
  override def prev = None
  override def withPrev(pattern: XPathPattern) = throw new UnsupportedOperationException()
  override def stepString = throw new UnsupportedOperationException()
  override def toString = "/"
}

case class AnyElement(prev: Option[XPathPattern]) extends XPathPattern {
  override def withPrev(pattern: XPathPattern) = AnyElement(Some(pattern))
  override def stepString = "*"
}

case class NamedElement(prev: Option[XPathPattern], name: String) extends XPathPattern {
  override def withPrev(pattern: XPathPattern) = NamedElement(Some(pattern), name)
  override def stepString = name
}

case class AnyAttribute(prev: Option[XPathPattern]) extends XPathPattern {
  override def withPrev(pattern: XPathPattern) = AnyAttribute(Some(pattern))
  override def stepString = "@*"
}

case class NamedAttribute(prev: Option[XPathPattern], name: String) extends XPathPattern {
  override def withPrev(pattern: XPathPattern) = NamedAttribute(Some(pattern), name)
  override def stepString = "@" + name
}