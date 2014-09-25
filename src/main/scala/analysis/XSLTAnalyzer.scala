package analysis

import xml.{XMLAttribute, XMLElement, XMLTextNode}
import xpath.NumberValue
import xslt._
import util.EvaluationError
import analysis.domain.{XMLDomain, XPathDomain}

/** Trait to analyze XSLT stylesheets using abstract interpretation */
trait XSLTAnalyzer[N, L, D1 <: XMLDomain[N, L], T, D2 <: XPathDomain[T, N, L, D1]] {

  val xmlDom: D1
  val xpathDom: D2
  val xpathAnalyzer: XPathAnalyzer[N, L, D1, T, D2]

  /** Transforms a source document (represented by it's root node) into a new document using an XSLT stylesheet*/
  def transform(sheet: XSLTStylesheet, source: N): N = {
    xmlDom.wrapInRoot(transform(sheet, xmlDom.liftList(List(source)), Map(), Map()))
  }

  /** Transforms a list of source nodes to a new list of nodes using given variable and parameter bindings */
  def transform(sheet: XSLTStylesheet, sources: L, variables: Map[String, T], params: Map[String, T]): L = {
    // create context, choose template, instantiate template, append results
    xpathDom.flatMapWithIndex(sources, (node, index) => {
      val templates = xmlDom.chooseTemplates(sheet, node)

      xmlDom.listJoin(templates.map { case (tmpl, specificNode) =>
        val context = AbstractXSLTContext[N, L, T](specificNode, sources, xpathDom.add(index, xpathDom.liftNumber(1)), variables)
        evaluateTemplate(sheet, tmpl, context, params)
      }.toList)
    })
  }

  /** Evaluates an XSLT template in a given XSLT context with parameters and returns a list of resulting nodes.
    *
    * @param tmpl the template to evaluate
    * @param context the context to evaluate the template in
    * @param params the parameters to use for evaluating the template (they will be ignored if they don't have
    *               a corresponding default parameter in the template, see XSLT spec section 11.6)
    * @return a list of resulting XML nodes
    */
  def evaluateTemplate(sheet: XSLTStylesheet, tmpl: XSLTTemplate, context: AbstractXSLTContext[N, L, T], params: Map[String, T]): L = {
    val acceptedParams = params.filter { case (key, _) => tmpl.defaultParams.contains(key) }
    val remainingDefaultParams = tmpl.defaultParams.filter { case (key, _) => !params.contains(key)}.mapValues(v => xpathAnalyzer.evaluate(v, xsltToXPathContext(context)))
    // the context for the newly instantiated template contains only global variables and parameters, no local parameters (static scoping)
    // TODO: insert global variables here, once they are supported
    evaluate(sheet, tmpl.content, context.replaceVariables(Map()).addVariables(remainingDefaultParams ++ acceptedParams))
  }

  /** Evaluates a sequence of XSLT instructions using a new scope (variable definitions are collected
    * so they will be visible in subsequent nodes, but never outside of the scope)
    *
    * @param nodes the instructions to evaluate
    * @param context the context to evaluate the first instruction in (subsequent instructions might have additional variable bindings)
    * @return a list of resulting XML nodes
    */
  def evaluate(sheet: XSLTStylesheet, nodes: Seq[XSLTInstruction], context: AbstractXSLTContext[N, L, T]): L = {
    // remember variable names that were created in this scope so we can throw an error
    // if any of these is shadowed in the SAME scope
    var scopeVariables = scala.collection.mutable.Set[String]()

    val (result, _) = nodes.foldLeft((xmlDom.liftList(Nil), context)) {
      case ((resultNodes, ctx), next) =>
        evaluate(sheet, next, ctx) match {
          case Left(moreResultNodes) => (xmlDom.listConcat(resultNodes, moreResultNodes), ctx)
          case Right((name, value)) =>
            if (scopeVariables.contains(name)) throw new EvaluationError(f"Variable $name is defined multiple times in the same scope")
            scopeVariables += name
            (resultNodes, ctx.addVariable(name, value))
        }
    }
    result
  }

  /** Evaluates a single XSLT instruction in a given XSLT context, resulting in either a list of result nodes
    * or an additional variable binding (if the instruction was a variable definition).
    */
  def evaluate(sheet: XSLTStylesheet, node: XSLTInstruction, context: AbstractXSLTContext[N, L, T]): Either[L, (String, T)] = {
    node match {
      case LiteralElement(name, children) =>
        val innerNodes = evaluate(sheet, children, context)
        val (resultAttributes, resultChildren) = xmlDom.partitionAttributes(innerNodes)
        var result = xmlDom.lift(XMLElement(name))
        result = xmlDom.addAttributes(result, resultAttributes)
        result = xmlDom.appendChildren(result, resultChildren)
        Left(xmlDom.liftList(List(result)))
      case LiteralTextNode(text) => Left(xmlDom.liftList(List(xmlDom.lift(XMLTextNode(text)))))
      case SetAttributeInstruction(attribute, value) =>
        // merge the content of all text-node children to create the attribute value
        val textResult = xpathDom.getConcatenatedTextNodeValues(evaluate(sheet, value, context))
        Left(xmlDom.liftList(List(xpathDom.liftAttribute(attribute, textResult))))
      case ApplyTemplatesInstruction(None, params) =>
        // TODO: what happens when template is applied on attribute or text node?
        Left(transform(sheet, xmlDom.getChildren(context.node), context.variables, params.mapValues(v => xpathAnalyzer.evaluate(v, xsltToXPathContext(context)))))
      case ApplyTemplatesInstruction(Some(expr), params) =>
        val result = xpathAnalyzer.evaluate(expr, xsltToXPathContext(context))
        val (extracted, _) = xpathDom.matchNodeSetValues(result)
        Left(transform(sheet, extracted, context.variables, params.mapValues(v => xpathAnalyzer.evaluate(v, xsltToXPathContext(context)))))
      case CallTemplatesInstruction(name, params) =>
        // unlike apply-templates, call-template does not change the current node or current node list (see spec section 6)
        Left(evaluateTemplate(sheet, sheet.namedTemplates(name), context, params.mapValues(v => xpathAnalyzer.evaluate(v, xsltToXPathContext(context)))))
      case VariableDefinitionInstruction(name, expr) =>
        Right(name, xpathAnalyzer.evaluate(expr, xsltToXPathContext(context)))
      case CopyInstruction(select) =>
        val evaluated = xpathAnalyzer.evaluate(select, xsltToXPathContext(context))
        val (nodeSets, rest) = xpathDom.matchNodeSetValues(evaluated)
        val nodeSetsOutput = xmlDom.copyToOutput(nodeSets)
        val restOutput = xmlDom.liftList(List(xpathDom.liftTextNode(xpathDom.toStringValue(rest))))
        Left(xmlDom.listJoin(nodeSetsOutput, restOutput))
      case ChooseInstruction(branches, otherwise) =>
        throw new NotImplementedError("Analyzing choose instructions is not implemented yet.")
        //Left(evaluate(sheet, chooseBranch(branches, otherwise, context.toXPathContext), context))

    }
  }

  /** Chooses the correct branch of a choose instruction.
    *
    * @param branches the remaining branches to test
    * @param otherwise the otherwise branch that will be evaluated when there is no other branch left
    * @param context the context to evaluate the instructions in
    * @return a list of resulting XML nodes
    */
  /*def chooseBranch(branches: List[(XPathExpr, Seq[XSLTInstruction])], otherwise: Seq[XSLTInstruction], context: XPathContext): Seq[XSLTInstruction] = {
    branches match {
      case Nil => otherwise
      case (firstExpr, firstTmpl) :: rest => XPathEvaluator.evaluate(firstExpr, context).toBooleanValue.value match {
        case true => firstTmpl
        case false => evaluateChoose(rest, otherwise, context)
      }
    }
  }*/

  def xsltToXPathContext(ctx: AbstractXSLTContext[N, L, T]): AbstractXPathContext[N, L, T] =
    AbstractXPathContext[N, L, T](ctx.node, ctx.position, xpathDom.getNodeListSize(ctx.nodeList), ctx.variables)
}
