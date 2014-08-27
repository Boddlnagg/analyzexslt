import scala.xml.{Comment, Text, Elem, Node}

/** Some utility constants and functions for working with XSLT */
object XSLT {
  /** The XSLT namespace */
  val Namespace = "http://www.w3.org/1999/XSL/Transform"

  // the concrete values here are irrelevant as long as user-defined > built-in
  val BuiltInImportPrecedence = -1
  val UserDefinedImportPrecedence = 0

  /** Returns a value indicating whether the given node is an element node and has the XSLT namespace */
  def isElem(node: Node): Boolean = {
    node.isInstanceOf[Elem] && node.namespace == Namespace
  }

  /** Returns a value indicating whether the given node is an element node with the specified name and has the XSLT namespace */
  def isElem(node: Node, name: String): Boolean = {
    isElem(node) && node.label == name
  }

  /** Preprocesses a [[scala.xml.Node]] by removing whitespace-only text-nodes (except inside &lt;xslt:text&gt;) and comment nodes. */
  def clean(x: Node): Node = x match {
    // This implementation is based on scala.xml.Utility.trim
    case Elem(pre, lab, md, scp, child@_*) =>
      val children = child flatMap cleanProper
      Elem(pre, lab, md, scp, children.isEmpty, children: _*)
  }

  def cleanProper(x: Node): Seq[Node] = x match {
    case Elem(pre, lab, md, scp, child@_*) =>
      // preserve whitespace inside <xsl:text> (see XSLT spec section 16.1)
      val children = if (XSLT.isElem(x, "text")) child else child flatMap cleanProper
      Elem(pre, lab, md, scp, children.isEmpty, children: _*)
    case Text(s) =>
      if (isOnlyWhitespace(s)) Nil else List(Text(s))
    case Comment(_) => Seq.empty // strip comments completely (see XSLT spec section 3)
    case _ =>
      x
  }

  /** Returns a value indicating whether the given string consists of only whitespace characters */
  def isOnlyWhitespace(s: String) = {
    val xmlWhitespace = List('\t', '\n', '\r', ' ') // according to XSLT spec section 3.4
    s.forall(c => xmlWhitespace.contains(c))
  }
}

/** An error that can be thrown when the evaluation of an XSLT template or an XPath expression fails */
class EvaluationError(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)