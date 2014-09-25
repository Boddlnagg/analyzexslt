package xslt

import xpath.{XPathExpr, XPathParser, LocationPath}

/** An XSLT stylesheet */
class XSLTStylesheet(templates: List[(XSLTTemplate, Option[String], Option[XPathExpr], ImportPrecedence)], disableBuiltinTemplates: Boolean) {
  templates.foreach  {
    case (_, None, None, _) => assert(false, "Templates must have either 'name' or 'match' attribute defined")
    case (_, _, Some(pattern), _) => assert(XPathExpr.isPattern(pattern), "Template 'match' attribute must be a pattern.")
    case _ => () // nothing to check
  }

  // Instead of having the original templates as a list, transform them to a Map[String, XSLTTemplate] for named
  // templates and a List[(LocationPath, Double, XSLTTemplate)] ordered by priority for those with 'match' attribute.
  // Union patterns are split so that every template is matched by exactly one location path.

  val namedTemplates: Map[String, XSLTTemplate] = Map() ++ templates.flatMap {
    case (tmpl, Some(name), _, _) => Some((name, tmpl))
    case _ => None
  }

  // add built-in templates (see spec section 5.8)
  val builtinTemplates =
    if (disableBuiltinTemplates)
      Nil
    else
      List[(XSLTTemplate, Option[String], Option[XPathExpr], ImportPrecedence)](
        (new XSLTTemplate(List(ApplyTemplatesInstruction())), None, Some(XPathParser.parse("*|/")), BuiltInImportPrecedence),
        // NOTE: <xsl:value-of select="."> is equivalent to <xsl:copy-of select="string(.)">
        (new XSLTTemplate(List(CopyInstruction(XPathParser.parse("string(.)")))), None, Some(XPathParser.parse("text()|@*")), BuiltInImportPrecedence),
        // NOTE: the XPath expression here originally is "processing-instruction()|comment()", but processing instructions are not implemented
        (new XSLTTemplate(Nil), None, Some(XPathParser.parse("comment()")), BuiltInImportPrecedence)
      )

  val matchableTemplates: List[(LocationPath, XSLTTemplate, Double, Int)] = (builtinTemplates ++ templates).flatMap {
    case (tmpl, _, Some(pattern), importPrecedence) => XPathExpr.splitUnionPattern(pattern).map(pat => (pat, tmpl, XPathExpr.getDefaultPriority(pat), importPrecedence))
    case _ => Nil
  }.sortBy { case (_, _, priority, importPrecedence) => (importPrecedence, priority) } // sort by precedence, then by priority
}