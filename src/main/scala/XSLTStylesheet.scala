import scala.xml._

/** A context for evaluating an XSLT template (see XSLT spec section 1)
  *
  * @param node the current node
  * @param nodeList the current node list
  * @param position the current node's position in the current node list
  * @param variables a set of variable bindings
  */
case class XSLTContext(node: XMLNode, nodeList: List[XMLNode], position: Int, variables: Map[String, XPathValue]) {
  /** Creates an equivalent XPath context from this XSLT context */
  def toXPathContext = XPathContext(node, position, nodeList.size, variables)

  /** Returns a new context where a variable binding was added to the set of variables */
  def addVariable(name: String, value: XPathValue): XSLTContext = {
    XSLTContext(node, nodeList, position, variables + (name -> value))
  }

  /** Returns a new context where a number of variable bindings were added to the set of variables */
  def addVariables(newVariables: Map[String, XPathValue]): XSLTContext = {
    XSLTContext(node, nodeList, position, variables ++ newVariables)
  }

  /** Returns a new context where the variable bindings were replaced by the given new set of variable */
  def replaceVariables(newVariables: Map[String, XPathValue]): XSLTContext = {
    XSLTContext(node, nodeList, position, newVariables)
  }
}

/** An XSLT stylesheet */
class XSLTStylesheet(var source: Elem) {
  val cleaned = XSLT.clean(source).asInstanceOf[Elem]

  if (cleaned.namespace != XSLT.Namespace) throw new NotImplementedError(f"Root element must be 'stylesheet' with namespace ${XSLT.Namespace} (a literal result element is not supported as root node)")
  if (cleaned.attribute("version").get.text != "1.0") throw new NotImplementedError("Stylesheet versions other than 1.0 are not supported")
  assert(cleaned.child.forall(n => n.namespace == XSLT.Namespace && (n.label == "template" || n.label == "variable")), "Top-level elements must either be XSLT templates or variable definitions")

  // TODO: the spec requires us to evaluate top-level variables in an order such that variables can depend on each other
  // (as long as there are no circular dependencies, which would result in an error), see spec section 11.4
  // -> don't support top-level variables?
  val globalVariables = cleaned.child
    .filter(XSLT.isElem(_, "variable"))
    .map(n => n.asInstanceOf[Elem])
    .map(elem => (elem.attribute("name").get.text, XPathExpr(elem.attribute("select").map(_.text).getOrElse("''"))))

  if (!globalVariables.isEmpty) throw new NotImplementedError("Top-level variables are not implemented.")

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
    (new XSLTTemplate(List(ApplyTemplatesInstruction())), None, Some(XPathExpr("*|/")), XSLT.BuiltInImportPrecedence),
    // NOTE: <xsl:value-of select="."> is equivalent to <xsl:copy-of select="string(.)">
    (new XSLTTemplate(List(CopyInstruction(XPathExpr("string(.)")))), None, Some(XPathExpr("text()|@*")), XSLT.BuiltInImportPrecedence),
    // NOTE: the XPath expression here originally is "processing-instruction()|comment()", but processing instructions are not implemented
    (new XSLTTemplate(Nil), None, Some(XPathExpr("comment()")), XSLT.BuiltInImportPrecedence)
  )

  val matchableTemplates = (builtinTemplates ++ templates).flatMap {
    case (tmpl, _, Some(pattern), importPrecedence) => XPathExpr.splitUnionPattern(pattern).map(pat => (pat, tmpl, XPathExpr.getDefaultPriority(pat), importPrecedence))
    case _ => Nil
  }.sortBy { case (_, _, priority, importPrecedence) => (importPrecedence, priority) } // sort by precedence, then by priority

  /** Transforms a source document (represented by it's root node) into a new document using this stylesheet*/
  def transform(source: XMLRoot): XMLRoot = {
    // process according to XSLT spec section 5.1
    transform(List(source), Map(), Map()) match {
      case List(elem@XMLElement(_, _, _, _)) => XMLRoot(elem)
      case _ => throw new IllegalStateException("Transformation result must be a single XMLElement")
    }
  }

  /** Transforms a list of source nodes to a new list of nodes using given variable and parameter bindings */
  def transform(sources: List[XMLNode], variables: Map[String, XPathValue], params: Map[String, XPathValue]) : List[XMLNode] = {
    // create context, choose template, instantiate template, append results
    sources.zipWithIndex
           .map { case (n,i) => (chooseTemplate(n), XSLTContext(n, sources, i + 1, variables)) }
           .flatMap { case (tmpl, context) => evaluateTemplate(tmpl, context, params) }
  }

  /** Chooses a template that matches the given element best */
  def chooseTemplate(elem: XMLNode) : XSLTTemplate = {
    def allMatching = matchableTemplates.filter { case (tmpl, _, _, _) => XPathMatcher.matches(elem, tmpl)}
    val (_, template, _, _) = allMatching.last // this one will have highest precedence and priority, because the templates are sorted
    template
  }

  /** Evaluates an XSLT template in a given XSLT context with parameters and returns a list of resulting nodes.
    *
    * @param tmpl the template to evaluate
    * @param context the context to evaluate the template in
    * @param params the parameters to use for evaluating the template (they will be ignored if they don't have
    *               a corresponding default parameter in the template, see XSLT spec section 11.6)
    * @return a list of resulting XML nodes
    */
  def evaluateTemplate(tmpl: XSLTTemplate, context: XSLTContext, params: Map[String, XPathValue]) : List[XMLNode] = {
    val acceptedParams = params.filter { case (key, _) => tmpl.defaultParams.contains(key) }
    val remainingDefaultParams = tmpl.defaultParams.filter { case (key, _) => !params.contains(key)}.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))
    // the context for the newly instantiated template contains only global variables and parameters, no local parameters (static scoping)
    // TODO: insert global variables here, once they are supported
    evaluate(tmpl.content, context.replaceVariables(Map()).addVariables(remainingDefaultParams ++ acceptedParams))
  }

  /** Evaluates a sequence of XSLT instructions using a new scope (variable definitions are collected
    * so they will be visible in subsequent nodes, but never outside of the scope)
    *
    * @param nodes the instructions to evaluate
    * @param context the context to evaluate the first instruction in (subsequent instructions might have additional variable bindings)
    * @return a list of resulting XML nodes
    */
  def evaluate(nodes: Seq[XSLTInstruction], context: XSLTContext) : List[XMLNode] = {
    // remember variable names that were created in this scope so we can throw an error
    // if any of these is shadowed in the SAME scope
    var scopeVariables = scala.collection.mutable.Set[String]()

    val (result, _) = nodes.foldLeft((List[XMLNode](), context)) {
      case ((resultNodes, ctx), next) =>
        evaluate(next, ctx) match {
          case Left(moreResultNodes) => (resultNodes ++ moreResultNodes, ctx)
          case Right((name, value)) =>
            if (scopeVariables.contains(name)) throw new EvaluationError(f"Variable $name is defined multiple times in the same scope")
            scopeVariables += name
            (resultNodes, ctx.addVariable(name, value))
        }
    }
    result
  }

  /** Evaluates a single XSLT instruction in a given XSLT node, resulting in either a list of result nodes
    * or an additional variable binding (if the instruction was a variable definition).
    */
  def evaluate(node: XSLTInstruction, context: XSLTContext): Either[List[XMLNode], (String, XPathValue)] = {
    node match {
      case LiteralElement(name, attributes, children) =>
        val resultNodes = evaluate(children, context)
        // attributes must come before all other result nodes, afterwards they are ignored (see spec section 7.1.3)
        // we also reverse their order to match the Java implementation (undefined in the spec)
        val resultAttributes = attributes ++ resultNodes
          .takeWhile(n => n.isInstanceOf[XMLAttribute])
          .map(n => n.asInstanceOf[XMLAttribute])
          .map(attr => (attr.name, attr.value))
          .reverse
        val resultChildren = resultNodes.filter(n => !n.isInstanceOf[XMLAttribute])
        Left(List(XMLElement(name,
          resultAttributes.map { case (key, value) => XMLAttribute(key, value)}.toSeq,
          resultChildren)))
      case LiteralTextNode(text) => Left(List(XMLTextNode(text)))
      case SetAttributeInstruction(attribute, value) =>
        // merge the content of all text-node children to create the attribute value
        val textResult = evaluate(value, context)
          .filter(n => n.isInstanceOf[XMLTextNode])
          .map(n => n.asInstanceOf[XMLTextNode].value)
          .mkString("")
        Left(List(XMLAttribute(attribute, textResult)))
      case ApplyTemplatesInstruction(None, params) =>
        context.node match {
          case root : XMLRoot => Left(transform(List(root.elem), context.variables, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
          case elem : XMLElement => Left(transform(elem.children.toList, context.variables, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
          case _ => Left(Nil) // other node types don't have children and return an empty result
        }
      case ApplyTemplatesInstruction(Some(expr), params) =>
        XPathEvaluator.evaluate(expr, context.toXPathContext) match {
          case NodeSetValue(nodes) => Left(transform(nodes, context.variables, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
          case value => throw new EvaluationError(f"select expression in apply-templates must evaluate to a node-set (evaluated to $value)")
        }
      case CallTemplatesInstruction(name, params) =>
        // unlike apply-templates, call-template does not change the current node or current node list (see spec section 6)
        Left(evaluateTemplate(namedTemplates(name), context, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
      case VariableDefinitionInstruction(name, expr) =>
        Right(name, XPathEvaluator.evaluate(expr, context.toXPathContext))
      case CopyInstruction(select) =>
        XPathEvaluator.evaluate(select, context.toXPathContext) match {
          // NOTE: result tree fragments are generally not supported
          case NodeSetValue(nodes) => Left(nodes.map(n => n.copy))
          case value => Left(List(XMLTextNode(value.toStringValue.value)))
        }
      case ChooseInstruction(branches, otherwise) =>
        Left(evaluate(evaluateChoose(branches, otherwise, context.toXPathContext), context))
    }
  }

  /** Evaluates the branches of a choose instruction.
    *
    * @param branches the remaining branches to test
    * @param otherwise the otherwise branch that will be evaluated when there is no other branch left
    * @param context the context to evaluate the instructions in
    * @return a list of resulting XML nodes
    */
  def evaluateChoose(branches: List[(XPathExpr, Seq[XSLTInstruction])], otherwise: Seq[XSLTInstruction], context: XPathContext): Seq[XSLTInstruction] = {
    branches match {
      case Nil => otherwise
      case (firstExpr, firstTmpl) :: rest => XPathEvaluator.evaluate(firstExpr, context).toBooleanValue.value match {
        case true => firstTmpl
        case false => evaluateChoose(rest, otherwise, context)
      }
    }
  }
}