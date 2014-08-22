import scala.xml.{Comment, Text, Elem, Node}

object XSLT {
  val Namespace = "http://www.w3.org/1999/XSL/Transform"

  // the concrete values here are irrelevant as long as user-defined > built-in
  val BuiltInImportPrecedence = -1
  val UserDefinedImportPrecedence = 0

  def isElem(node: Node): Boolean = {
    node.isInstanceOf[Elem] && node.namespace == Namespace
  }

  def isElem(node: Node, name: String): Boolean = {
    isElem(node) && node.label == name
  }

  /** This is based on scala.xml.Utility.trim */
  def clean(x: Node): Node = x match {
    case Elem(pre, lab, md, scp, child@_*) =>
      val children = child flatMap cleanProper
      Elem(pre, lab, md, scp, children.isEmpty, children: _*)
  }

  def cleanProper(x: Node): Seq[Node] = x match {
    case Elem(pre, lab, md, scp, child@_*) =>
      // preserve whitespace inside <xsl:text>
      val children = if (XSLT.isElem(x, "text")) child else child flatMap cleanProper
      Elem(pre, lab, md, scp, children.isEmpty, children: _*)
    case Text(s) =>
      if (isOnlyWhitespace(s)) Nil else List(Text(s))
    case Comment(_) => Seq.empty // strip comments completely (see spec section 3)
    case _ =>
      x
  }

  def isOnlyWhitespace(s: String) = {
    val xmlWhitespace = List('\t', '\n', '\r', ' ') // according to XSLT spec section 3.4
    s.forall(c => xmlWhitespace.contains(c))
  }
}