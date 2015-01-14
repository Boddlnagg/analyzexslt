package xslt

import xpath.{XPathExpr, XPathParser, LocationPath}

/** An XSLT stylesheet responsible for managing its named and matchable templates. */
class XSLTStylesheet(templates: List[(XSLTTemplate, Option[String], Option[XPathExpr])], disableBuiltinTemplates: Boolean) {
  // perform some sanity checks
  templates.foreach  {
    case (_, None, None) => assert(false, "Templates must have either 'name' or 'match' attribute defined")
    case (_, _, Some(pattern)) => assert(XPathExpr.isPattern(pattern), "Template 'match' attribute must be a pattern.")
    case _ => () // nothing to check
  }

  // Instead of having the original templates as a list, transform them to a Map[String, XSLTTemplate] for named
  // templates and a List[(LocationPath, XSLTTemplate, Double, Int)] ordered by precedence/priority for those with
  // 'match'-clause. Union patterns are split so that every template is matched by exactly one location path.

  val namedTemplates: Map[String, XSLTTemplate] = Map() ++ templates.collect {
    case (tmpl, Some(name), _) => (name, tmpl)
  }

  private val userDefinedMatchableTemplates =
    templates.collect { case (tmpl, _, Some(pattern)) => (tmpl, pattern, UserDefinedImportPrecedence) }

  // add built-in templates (see spec section 5.8)
  private val builtinTemplates: List[(XSLTTemplate, XPathExpr, ImportPrecedence)] =
    if (disableBuiltinTemplates)
      Nil
    else
      List(
        (new XSLTTemplate(List(ApplyTemplatesInstruction())), XPathParser.parse("*|/"), BuiltInImportPrecedence),
        // NOTE: <xsl:value-of select="."> is equivalent to <xsl:copy-of select="string(.)">
        (new XSLTTemplate(List(CopyInstruction(XPathParser.parse("string(.)")))), XPathParser.parse("text()|@*"), BuiltInImportPrecedence),
        // NOTE: the XPath expression here originally is "processing-instruction()|comment()", but processing instructions are not implemented
        (new XSLTTemplate(Nil), XPathParser.parse("comment()"), BuiltInImportPrecedence)
      )

  val matchableTemplates: List[(LocationPath, XSLTTemplate)] =
    (builtinTemplates ++ userDefinedMatchableTemplates).flatMap {
      case (tmpl, pattern, importPrecedence) => XPathExpr.splitUnionPattern(pattern).map(pat => (pat, tmpl, XPathExpr.getDefaultPriority(pat), importPrecedence))
    }.sortBy {
      case (_, _, priority, importPrecedence) => (importPrecedence, priority) // sort by precedence, then by priority
    }.map {
      case (path, tmpl, _, _) => (path, tmpl) // forget about precedence and priority
    }.reverse // reverse the list, so that the first element has highest precedence and priority
}