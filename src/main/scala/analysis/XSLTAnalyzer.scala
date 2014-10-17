package analysis

import xpath.XPathExpr
import xslt._
import util.EvaluationError
import analysis.domain.Domain
import scala.util.control.Breaks._

/** Trait to analyze XSLT stylesheets using abstract interpretation */
class XSLTAnalyzer[N, L, V](dom: Domain[N, L, V]) {

  val xmlDom = dom.xmlDom
  val xpathDom = dom.xpathDom
  val xpathAnalyzer = new XPathAnalyzer[N, L, V](dom)
  val xpathMatcher = new AbstractXPathMatcher[N, L, V](dom)

  /** Transforms a source document (represented by it's root node) into a new document using an XSLT stylesheet*/
  def transform(sheet: XSLTStylesheet, source: N): N = {
    xmlDom.wrapInRoot(transform(sheet, xmlDom.createSingletonList(source), Map(), Map()))
  }

  /** Transforms a list of source nodes to a new list of nodes using given variable and parameter bindings */
  def transform(sheet: XSLTStylesheet, sources: L, variables: Map[String, V], params: Map[String, V]): L = {
    // create context, choose template, instantiate template, append results
    xmlDom.flatMapWithIndex(sources, (node, index) => {
      val templates = chooseTemplates(sheet, node)

      xmlDom.joinList(templates.map { case (tmpl, specificNode) =>
        val context = AbstractXSLTContext[N, L, V](specificNode, sources, xpathDom.add(index, xpathDom.liftNumber(1)), variables)
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
  def evaluateTemplate(sheet: XSLTStylesheet, tmpl: XSLTTemplate, context: AbstractXSLTContext[N, L, V], params: Map[String, V]): L = {
    val acceptedParams = params.filter { case (key, _) => tmpl.defaultParams.contains(key) }
    val remainingDefaultParams = tmpl.defaultParams.filter { case (key, _) => !params.contains(key)}.mapValues(v => xpathAnalyzer.evaluate(v, xsltToXPathContext(context)))
    // the context for the newly instantiated template contains only global variables and parameters, no local parameters (static scoping)
    // TODO: insert global variables here, once they are supported
    evaluate(sheet, tmpl.content, context.replaceVariables(Map()).addVariables(remainingDefaultParams ++ acceptedParams))
  }

  def chooseTemplates(sheet: XSLTStylesheet, node: N): Map[XSLTTemplate, N] = {
    // don't know anything -> return set of all matchable templates
    //case None => sheet.matchableTemplates.map { case (_, tmpl, _, _) => (tmpl, n)}.toMap
    val result = scala.collection.mutable.Map[XSLTTemplate, N]()
    var currentNode = node
    breakable {
      sheet.matchableTemplates.reverse.foreach { case (path, tpl, _, _) =>
        val (matches, notMatches) = xpathMatcher.matches(currentNode, path)
        if (xmlDom.compare(matches, xmlDom.bottom) == Greater)
          result.put(tpl, matches)
        if (xmlDom.compare(notMatches, xmlDom.bottom) == Equal)
          break()

        // TODO: use comparison instead of second match result?
        //if (node < matches)
        //  break()

        currentNode = notMatches
      }
    }
    result.toMap
  }

  /** Evaluates a sequence of XSLT instructions using a new scope (variable definitions are collected
    * so they will be visible in subsequent nodes, but never outside of the scope)
    *
    * @param nodes the instructions to evaluate
    * @param context the context to evaluate the first instruction in (subsequent instructions might have additional variable bindings)
    * @return a list of resulting XML nodes
    */
  def evaluate(sheet: XSLTStylesheet, nodes: Seq[XSLTInstruction], context: AbstractXSLTContext[N, L, V]): L = {
    // remember variable names that were created in this scope so we can throw an error
    // if any of these is shadowed in the SAME scope
    var scopeVariables = scala.collection.mutable.Set[String]()

    val (result, _) = nodes.foldLeft((xmlDom.createEmptyList(), context)) {
      case ((resultNodes, ctx), next) =>
        evaluate(sheet, next, ctx) match {
          case Left(moreResultNodes) => (xmlDom.concatLists(resultNodes, moreResultNodes), ctx)
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
  def evaluate(sheet: XSLTStylesheet, node: XSLTInstruction, context: AbstractXSLTContext[N, L, V]): Either[L, (String, V)] = {
    node match {
      case LiteralElement(name, children) =>
        val innerNodes = evaluate(sheet, children, context)
        val (resultAttributes, resultChildren) = xmlDom.partitionAttributes(innerNodes)
        val result = xmlDom.createElement(name, resultAttributes, resultChildren)
        Left(xmlDom.createSingletonList(result))
      case LiteralTextNode(text) => Left(xmlDom.createSingletonList(xmlDom.createTextNode(xpathDom.liftLiteral(text))))
      case SetAttributeInstruction(attribute, value) =>
        // merge the content of all text-node children to create the attribute value
        val textResult = xmlDom.getConcatenatedTextNodeValues(evaluate(sheet, value, context))
        Left(xmlDom.createSingletonList(xmlDom.createAttribute(attribute, textResult)))
      case ApplyTemplatesInstruction(None, params) =>
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
        val restOutput = xmlDom.createSingletonList(xmlDom.createTextNode(xpathDom.toStringValue(rest)))
        Left(xmlDom.joinList(nodeSetsOutput, restOutput))
      case ChooseInstruction(branches, otherwise) =>
        val possibleBranches = chooseBranches(branches, otherwise, xsltToXPathContext(context))
        // evaluate all possible branches and join the result lists
        Left(xmlDom.joinList(possibleBranches.map(br => evaluate(sheet, br, context)).toList))

    }
  }

  /** Chooses the possible branches of a choose instruction.
    *
    * @param branches the remaining branches to test
    * @param otherwise the otherwise branch that will be evaluated when there is no other branch left
    * @param context the context to evaluate the instructions in
    * @return a set of possible branches, represented by their body as XSLT instructions
    */
  def chooseBranches(branches: List[(XPathExpr, Seq[XSLTInstruction])], otherwise: Seq[XSLTInstruction], context: AbstractXPathContext[N, L, V]): Set[Seq[XSLTInstruction]] = {
    branches match {
      case Nil => Set(otherwise)
      case (firstExpr, firstTmpl) :: rest =>
        val result = xpathDom.toBooleanValue(xpathAnalyzer.evaluate(firstExpr, context))
        val maybeTrue = xpathDom.compare(result, xpathDom.liftBoolean(true)) match {
          case Greater | Equal => true // the value may be true -> evaluate this branch
          case _ => false
        }
        val maybeFalse = xpathDom.compare(result, xpathDom.liftBoolean(false)) match {
          case Greater | Equal => true // the value may be false -> evaluate further branches after this one
          case _ => false
        }
        (maybeTrue, maybeFalse) match {
          case (true, true) => Set(firstTmpl) ++ chooseBranches(rest, otherwise, context)
          case (true, false) => Set(firstTmpl)
          case (false, true) => chooseBranches(rest, otherwise, context)
          case (false, false) => Set[Seq[XSLTInstruction]]()
        }
    }
  }

  def xsltToXPathContext(ctx: AbstractXSLTContext[N, L, V]): AbstractXPathContext[N, L, V] =
    AbstractXPathContext[N, L, V](ctx.node, ctx.position, xmlDom.getNodeListSize(ctx.nodeList), ctx.variables)
}
