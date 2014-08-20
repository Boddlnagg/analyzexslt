import scala.xml._

object XSLT {
  val Namespace = "http://www.w3.org/1999/XSL/Transform"
  val BuiltInImportPrecedence = -1
  val UserDefinedImportPrecedence = 0

  def isElem(node: Node): Boolean = {
    node.isInstanceOf[Elem] && node.namespace == Namespace
  }

  def isElem(node: Node, name: String): Boolean = {
    isElem(node) && node.label == name
  }
}

class XSLTStylesheet(var source: Elem) {
  val cleaned = clean(source).asInstanceOf[Elem]

  assert(cleaned.namespace == XSLT.Namespace, f"Root element must be 'stylesheet' with namespace ${XSLT.Namespace} (a literal result element is not supported as root node)")
  assert(cleaned.attribute("version").get.text == "1.0", "Stylesheet version must be 1.0")
  assert(cleaned.child.forall(n => n.namespace == XSLT.Namespace && (n.label == "template" || n.label == "variable")), "Top-level elements must either be XSLT templates or variable definitions")

  // TODO: the spec requires us to evaluate variables in an order such that variables can depend on each other (as long
  // as there are no circular dependencies, which would result in an error), see spec section 11.4
  // var topLevelVariables = ...

  val templates = cleaned.child
    .filter(XSLT.isElem(_, "template"))
    .map(n => n.asInstanceOf[Elem])
    .map(elem => (XSLTTemplate(elem),
                  elem.attribute("name").map(_.text),
                  elem.attribute("match").map(a => XPathExpr(a.text)),
                  XSLT.UserDefinedImportPrecedence
                 ))
    .toList // returns a list of tuples (template, name?, match?, precedence)

  templates.foreach  {
    case (_, None, None, _) => assert(false, "Templates must have either 'name' or 'match' attribute defined")
    case (_, _, Some(pattern), _) => assert(XPathExpr.isPattern(pattern), "Template 'match' attribute must be a pattern.")
    case _ => () // nothing to check
  }

  // Instead of having the original templates as a list, transform them to a Map[String, XSLTTemplate] for named
  // templates and a List[(LocationPath, Double, XSLTTemplate)] ordered by priority for those with 'match' attribute.
  // Union patterns are split so that every template is matched by exactly one location path.

  val namedTemplates = Map() ++ templates.flatMap {
    case (tmpl, Some(name), _, _) => Some((name, tmpl))
    case _ => None
  }

  // add built-in templates (see spec section 5.8)
  val builtinTemplates = List[(XSLTTemplate, Option[String], Option[XPathExpr], Int)](
    (new XSLTTemplate(List(ApplyTemplatesElement())), None, Some(XPathExpr("*|/")), XSLT.BuiltInImportPrecedence),
    (new XSLTTemplate(List(ValueOfElement(XPathExpr(".")))), None, Some(XPathExpr("text()|@*")), XSLT.BuiltInImportPrecedence),
    (new XSLTTemplate(Nil), None, Some(XPathExpr("processing-instruction()|comment()")), XSLT.BuiltInImportPrecedence)
  )

  val matchableTemplates = (builtinTemplates ++ templates).flatMap {
    case (tmpl, _, Some(pattern), importPrecedence) => XPathExpr.splitUnionPattern(pattern).map(pat => (pat, tmpl, XPathExpr.getDefaultPriority(pat), importPrecedence))
    case _ => Nil
  }.sortBy { case (_, _, priority, importPrecedence) => (importPrecedence, priority) }

  def transform(source: XMLElement): XMLElement = {
    // TODO
    source
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
      new TextBuffer().append(s).toText
    case Comment(_) => Seq.empty // strip comments completely (see spec section 3)
    case _ =>
      x
  }
}