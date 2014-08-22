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

case class XSLTContext(node: XMLNode, nodeList: List[XMLNode], position: Int)

class XSLTStylesheet(var source: Elem) {
  val cleaned = clean(source).asInstanceOf[Elem]

  assert(cleaned.namespace == XSLT.Namespace, f"Root element must be 'stylesheet' with namespace ${XSLT.Namespace} (a literal result element is not supported as root node)")
  assert(cleaned.attribute("version").get.text == "1.0", "Stylesheet version must be 1.0")
  assert(cleaned.child.forall(n => n.namespace == XSLT.Namespace && (n.label == "template" || n.label == "variable")), "Top-level elements must either be XSLT templates or variable definitions")

  // TODO: the spec requires us to evaluate top-level variables in an order such that variables can depend on each other
  // (as long as there are no circular dependencies, which would result in an error), see spec section 11.4
  // -> don't support top-level variables?
  val variables = cleaned.child
    .filter(XSLT.isElem(_, "variable"))
    .map(n => n.asInstanceOf[Elem])

  assert(variables.isEmpty, "Top-level variables are currently not supported.")

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
    // NOTE: <xsl:value-of select="."> is equivalent to <xsl:copy-of select="string(.)">
    (new XSLTTemplate(List(CopyOfElement(XPathExpr("string(.)")))), None, Some(XPathExpr("text()|@*")), XSLT.BuiltInImportPrecedence),
    // NOTE: the XPath expression here originally is "processing-instruction()|comment()", but processing instructions are not implemented
    (new XSLTTemplate(Nil), None, Some(XPathExpr("comment()")), XSLT.BuiltInImportPrecedence)
  )

  val matchableTemplates = (builtinTemplates ++ templates).flatMap {
    case (tmpl, _, Some(pattern), importPrecedence) => XPathExpr.splitUnionPattern(pattern).map(pat => (pat, tmpl, XPathExpr.getDefaultPriority(pat), importPrecedence))
    case _ => Nil
  }.sortBy { case (_, _, priority, importPrecedence) => (importPrecedence, priority) } // sort by precedence, then by priority

  def transform(source: XMLRoot): XMLRoot = {
    // process according to XSLT spec section 5.1
    transform(List(source)) match {
      case List(elem@XMLElement(_, _, _, _)) => XMLRoot(elem)
      case _ => throw new IllegalStateException("Transformation result must be a single XMLElement")
    }
  }

  def transform(sources: List[XMLNode]) : List[XMLNode] = {
    // create context, choose template, instantiate template, append results
    sources.zipWithIndex
           .map { case (n,i) => (chooseTemplate(n), XSLTContext(n, sources, i + 1)) }
           .flatMap { case (tmpl, context) => evaluate(tmpl, context) }
  }

  def chooseTemplate(elem: XMLNode) : XSLTTemplate = {
    def allMatching = matchableTemplates.filter { case (tmpl, _, _, _) => XPathMatcher.matches(elem, tmpl)}
    val (_, template, _, _) = allMatching.last // this one will have highest precedence and priority, because the templates are sorted
    template
  }

  def evaluate(tmpl: XSLTTemplate, context: XSLTContext) : List[XMLNode] = {
    evaluate(tmpl.content, context)
  }

  def evaluate(nodes: Seq[XSLTNode], context: XSLTContext) : List[XMLNode] = {
    nodes.flatMap(n => evaluate(n, context)).toList
  }

  def evaluate(node: XSLTNode, context: XSLTContext): List[XMLNode] = {
    node match {
      case LiteralElement(name, attributes, children) =>
        val resultNodes = children.flatMap(n => evaluate(n, context))
        // attributes must come before all other result nodes, afterwards they are ignored (see spec section 7.1.3)
        val resultAttributes = attributes ++ resultNodes
          .takeWhile(n => n.isInstanceOf[XMLAttribute])
          .map(n => n.asInstanceOf[XMLAttribute])
          .map(attr => (attr.name, attr.value))
        val resultChildren = resultNodes.filter(n => !n.isInstanceOf[XMLAttribute])
        List(XMLElement(name,
          resultAttributes.map { case (key, value) => XMLAttribute(key, value)}.toSeq,
          resultChildren))
      case LiteralTextNode(text) => List(XMLTextNode(text))
      case SetAttributeElement(attribute, value) =>
        // merge the content of all text-node children to create the attribute value
        val textResult = evaluate(value, context)
          .filter(n => n.isInstanceOf[XMLTextNode])
          .map(n => n.asInstanceOf[XMLTextNode].value)
          .mkString("")
        List(XMLAttribute(attribute, textResult))
      // TODO: support select attribute for apply-templates
      case ApplyTemplatesElement(None, params) =>
        context.node match {
          case root : XMLRoot => transform(List(root.elem))
          case elem : XMLElement => transform(elem.children.toList)
          case _ => Nil // other node types don't have children and return an empty result
        }
      case _ => throw new UnsupportedOperationException(f"Evaluation of $node is not implemented.")
    }
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