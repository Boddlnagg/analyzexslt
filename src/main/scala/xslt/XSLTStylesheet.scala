package xslt

import xpath.{XPathExpr, XPathParser, LocationPath}

/** An XSLT stylesheet responsible for managing its named and matchable templates. */
class XSLTStylesheet(
  templates: List[(XSLTTemplate, Option[String], Option[XPathExpr], Option[String])],
  val globalVariables: List[(String, Either[XPathExpr, Seq[XSLTInstruction]])],
  disableBuiltinTemplates: Boolean
) {
  // templates is a list of tuples of the form (template, name?, match-clause?, mode?)

  // perform some sanity checks
  templates.foreach  {
    case (_, None, None, _) => assert(false, "Templates must have either 'name' or 'match' attribute defined")
    case (_, _, Some(pattern), _) => assert(XPathExpr.isPattern(pattern), "Template 'match' attribute must be a pattern.")
    case _ => () // nothing to check
  }

  // Instead of having the original templates as a list, transform them to a Map[String, XSLTTemplate] for named
  // templates and a List[(LocationPath, XSLTTemplate, Double, Int)] ordered by precedence/priority for those with
  // 'match'-clause. Union patterns are split so that every template is matched by exactly one location path.

  val namedTemplates: Map[String, XSLTTemplate] = Map() ++ templates.collect {
    case (tmpl, Some(name), _, _) => (name, tmpl)
  }

  private val userDefinedMatchableTemplates =
    templates.collect { case (tmpl, _, Some(pattern), mode) => (tmpl, pattern, mode, UserDefinedImportPrecedence) }
      .groupBy { case (_, _, mode, _) => mode }
      .mapValues(_.map { case (tmpl, pattern, _, precedence ) => (tmpl, pattern, precedence) })

  private val allModes: Set[Option[String]] = userDefinedMatchableTemplates.keys.toSet | Set(None)

  // add built-in templates (see spec section 5.8)
  private def getBuiltinTemplates(mode: Option[String]): List[(XSLTTemplate, XPathExpr, ImportPrecedence)] =
    if (disableBuiltinTemplates)
      Nil
    else
      List(
        (new XSLTTemplate(List(ApplyTemplatesInstruction(None, mode))), XPathParser.parse("*|/"), BuiltInImportPrecedence),
        // NOTE: <xsl:value-of select="."> is equivalent to <xsl:copy-of select="string(.)">
        (new XSLTTemplate(List(CopyOfInstruction(XPathParser.parse("string(.)")))), XPathParser.parse("text()|@*"), BuiltInImportPrecedence),
        // NOTE: the XPath expression here originally is "processing-instruction()|comment()", but processing instructions are not implemented
        (new XSLTTemplate(Nil), XPathParser.parse("comment()"), BuiltInImportPrecedence)
      )

  val matchableTemplates: Map[Option[String], List[(LocationPath, XSLTTemplate)]] =
    Map() ++ allModes.map { mode =>
      mode -> (getBuiltinTemplates(mode) ++ userDefinedMatchableTemplates.getOrElse(mode, Nil)).flatMap {
        case (tmpl, pattern, importPrecedence) => XPathExpr.splitUnionPattern(pattern).map(pat => (pat, tmpl, XPathExpr.getDefaultPriority(pat), importPrecedence))
      }.sortBy {
        case (_, _, priority, importPrecedence) => (importPrecedence, priority) // sort by precedence, then by priority
      }.map {
        case (path, tmpl, _, _) => (path, tmpl) // forget about precedence and priority
      }.reverse // reverse order, so highest precedence comes first
    }
}