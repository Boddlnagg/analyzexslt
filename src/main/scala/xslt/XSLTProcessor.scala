package xslt

import util.ProcessingError
import xml._
import xpath._

/** Object to process XSLT stylesheets. */
object XSLTProcessor {
  /** Transforms a source document (represented by it's root node) into a new document using an XSLT stylesheet*/
  def transform(sheet: XSLTStylesheet, source: XMLRoot): XMLRoot = {
    // process according to XSLT spec section 5.1
    transform(sheet, List(source), Map(), Map()) match {
      case List(inner@XMLElement(_, _, _, _)) => XMLRoot(inner)
      case _ => throw new IllegalStateException("Transformation result must be a single XMLElement")
    }
  }

  /** Transforms a list of source nodes to a new list of nodes using given variable and parameter bindings */
  def transform(sheet: XSLTStylesheet, sources: List[XMLNode], variables: Map[String, XPathValue], params: Map[String, XPathValue]): List[XMLNode] = {
    // create context, choose template, instantiate template, append results
    sources.zipWithIndex.flatMap { case (n,i) =>
      val tmpl = chooseTemplate(sheet, n)
      val context = XSLTContext(n, sources, i + 1, variables)
      instantiateTemplate(sheet, tmpl, context, params)
    }
  }

  /** Chooses a template that matches the given element best */
  def chooseTemplate(sheet: XSLTStylesheet, node: XMLNode): XSLTTemplate = {
    val allMatching = sheet.matchableTemplates.filter { case (tmpl, _, _, _) => XPathMatcher.matches(node, tmpl)}
    if (allMatching.isEmpty)
      throw new ProcessingError(f"Found no matching template for input node `${XMLNode.formatPath(node)}` [NOTE: this can only happen when builtin templates are disabled]")
    val (_, template, _, _) = allMatching.last // this one will have highest precedence and priority, because the templates are sorted
    template
  }

  /** Instantiates an XSLT template in a given XSLT context with parameters and returns a list of resulting nodes.
    *
    * @param tmpl the template to instantiate
    * @param context the context used to instantiate the template
    * @param params the parameters to use for instantiating the template (they will be ignored if they don't have
    *               a corresponding default parameter in the template, see XSLT spec section 11.6)
    * @return a list of resulting XML nodes
    */
  def instantiateTemplate(sheet: XSLTStylesheet, tmpl: XSLTTemplate, context: XSLTContext, params: Map[String, XPathValue]): List[XMLNode] = {
    val acceptedParams = params.filter { case (key, _) => tmpl.defaultParams.contains(key) }
    val remainingDefaultParams = tmpl.defaultParams.filter { case (key, _) => !params.contains(key)}.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))
    // the context for the newly instantiated template contains only global variables and parameters, no local parameters
    // (static scoping and no nested template definitions); global variables are not supported in this implementation
    process(sheet, tmpl.content, context.replaceVariables(Map()).addVariables(remainingDefaultParams ++ acceptedParams))
  }

  /** Processes a sequence of XSLT instructions using a new scope (variable definitions are collected
    * so they will be visible in subsequent nodes, but never outside of the scope)
    *
    * @param nodes the instructions to process
    * @param context the context to evaluate the first instruction in (subsequent instructions might have additional variable bindings)
    * @return a list of resulting XML nodes
    */
  def process(sheet: XSLTStylesheet, nodes: Seq[XSLTInstruction], context: XSLTContext): List[XMLNode] = {
    // remember variable names that were created in this scope so we can throw an error
    // if any of these is shadowed in the SAME scope
    var scopeVariables = scala.collection.mutable.Set[String]()

    val (result, _) = nodes.foldLeft((List[XMLNode](), context)) {
      case ((resultNodes, ctx), next) =>
        process(sheet, next, ctx) match {
          case Left(moreResultNodes) => (resultNodes ++ moreResultNodes, ctx)
          case Right((name, value)) =>
            if (scopeVariables.contains(name)) throw new ProcessingError(f"Variable $name is defined multiple times in the same scope")
            scopeVariables += name
            (resultNodes, ctx.addVariable(name, value))
        }
    }
    result
  }

  /** Processes a single XSLT instruction in a given XSLT context, resulting in either a list of result nodes
    * or an additional variable binding (if the instruction was a variable definition).
    */
  def process(sheet: XSLTStylesheet, node: XSLTInstruction, context: XSLTContext): Either[List[XMLNode], (String, XPathValue)] = {
    node match {
      case CreateElementInstruction(name, children) =>
        val resultNodes = process(sheet, children, context)
        // attributes must come before all other result nodes, afterwards they are ignored (see spec section 7.1.3)
        val resultAttributes = resultNodes.takeWhile(n => n.isInstanceOf[XMLAttribute]).map(n => n.asInstanceOf[XMLAttribute])
        val resultChildren = resultNodes.filter(n => !n.isInstanceOf[XMLAttribute])
        Left(List(XMLElement(name, resultAttributes, resultChildren)))
      case CreateTextInstruction(text) =>
        if (text != "")
          Left(List(XMLTextNode(text)))
        else
          Left(List()) // no text node will be created for the empty string
      case CreateCommentInstruction(value) =>
        // merge the content of all text-node children to create the comment value (non-text-node children are wrong and can be ignored according to spec)
        val textResult = process(sheet, value, context)
          .collect { case n: XMLTextNode => n.value }
          .mkString("")
        Left(List(XMLComment(textResult)))
      case SetAttributeInstruction(attribute, value) =>
        // merge the content of all text-node children to create the attribute value (non-text-node children are wrong and can be ignored according to spec)
        val textResult = process(sheet, value, context)
          .collect { case n: XMLTextNode => n.value }
          .mkString("")
        Left(List(XMLAttribute(attribute, textResult)))
      case ApplyTemplatesInstruction(None, params) =>
        context.node match {
          case XMLRoot(inner) => Left(transform(sheet, List(inner), context.variables, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
          case elem: XMLElement => Left(transform(sheet, elem.children.toList, context.variables, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
          case _ => Left(Nil) // other node types don't have children and return an empty result
        }
      case ApplyTemplatesInstruction(Some(expr), params) =>
        XPathEvaluator.evaluate(expr, context.toXPathContext) match {
          case NodeSetValue(nodes) => Left(transform(sheet, nodes.toList, context.variables, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
          case value => throw new ProcessingError(f"select expression in apply-templates must evaluate to a node-set (evaluated to $value)")
        }
      case CallTemplatesInstruction(name, params) =>
        // unlike apply-templates, call-template does not change the current node or current node list (see spec section 6)
        Left(instantiateTemplate(sheet, sheet.namedTemplates(name), context, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
      case VariableDefinitionInstruction(name, expr) =>
        Right(name, XPathEvaluator.evaluate(expr, context.toXPathContext))
      case CopyInstruction(select) =>
        XPathEvaluator.evaluate(select, context.toXPathContext) match {
          // NOTE: result tree fragments are generally not supported
          case NodeSetValue(nodes) => Left(nodes.toList.map {
            case XMLRoot(inner) => inner.copy // "a root node is copied by copying its children" according to spec
            case node => node.copy
          })
          case value =>
            val textValue = value.toStringValue.value
            if (textValue != "")
              Left(List(XMLTextNode(value.toStringValue.value)))
            else
              Left(List()) // text nodes with empty content are not allowed
        }
      case ChooseInstruction(branches, otherwise) =>
        Left(process(sheet, chooseBranch(branches, otherwise, context.toXPathContext), context))
    }
  }

  /** Evaluates the branches of a choose instruction.
    *
    * @param branches the remaining branches to test
    * @param otherwise the otherwise branch that will be evaluated when there is no other branch left
    * @param context the context to evaluate the instructions in
    * @return a list of resulting XSLT instructions representing the body of the branch
    */
  def chooseBranch(branches: List[(XPathExpr, Seq[XSLTInstruction])], otherwise: Seq[XSLTInstruction], context: XPathContext): Seq[XSLTInstruction] = {
    branches match {
      case Nil => otherwise
      case (firstExpr, firstTmpl) :: rest => XPathEvaluator.evaluate(firstExpr, context).toBooleanValue.value match {
        case true => firstTmpl
        case false => chooseBranch(rest, otherwise, context)
      }
    }
  }
}
