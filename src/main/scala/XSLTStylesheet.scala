import scala.xml._

object XSLT {
  val Namespace = "http://www.w3.org/1999/XSL/Transform";

  def isElem(node: Node): Boolean = {
    node.isInstanceOf[Elem] && node.namespace == Namespace
  }

  def isElem(node: Node, name: String): Boolean = {
    isElem(node) && node.label == name
  }
}

class XSLTStylesheet(var source: Elem) {
  // the content of xsl:text should not be trimmed, but is currently not supported anyway
  val cleaned = clean(source).asInstanceOf[Elem]

  assert(cleaned.namespace == XSLT.Namespace, f"Root element must be 'stylesheet' with namespace ${XSLT.Namespace} (a literal result element is not supported as root node)");
  assert(cleaned.attribute("version").get.text == "1.0", "Stylesheet version must be 1.0");
  assert(cleaned.child.forall(n => n.namespace == XSLT.Namespace && (n.label == "template" || n.label == "variable")), "Top-level elements must either be XSLT templates or variable definitions");

  // TODO: the spec requires us to evaluate variables in an order such that variables can depend on each other (as long
  // as there are no circular dependencies, which would result in an error), see spec section 11.4
  // var topLevelVariables = ...
  var templates = cleaned.child
    .filter(XSLT.isElem(_, "template"))
    .map(n => n.asInstanceOf[Elem])
    .map(elem => XSLTTemplate.parse(elem)).toList;

  // TODO: implement built-in template rules (spec section 5.8)

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
      new TextBuffer().append(s).toText
    case Comment(_) => Seq.empty // strip comments completely (see spec section 3)
    case _ =>
      x
  }
}