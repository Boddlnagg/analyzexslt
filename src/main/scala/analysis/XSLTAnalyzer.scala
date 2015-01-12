package analysis

import xpath.XPathExpr
import xslt._
import util.ProcessingError
import analysis.domain.Domain
import scala.util.control.Breaks._

/** Class to analyze XSLT stylesheets using abstract interpretation */
class XSLTAnalyzer[N, L, V](dom: Domain[N, L, V]) {

  val xmlDom = dom.xmlDom
  val xpathDom = dom.xpathDom
  val xpathAnalyzer = new XPathAnalyzer[N, L, V](dom)
  val xpathMatcher = new AbstractXPathMatcher[N, L, V](xmlDom)

  /** Transforms a source document (represented by it's root node) into a new document using an XSLT stylesheet*/
  def transform(sheet: XSLTStylesheet, source: N): N = {
    val (rootSource, _) = xmlDom.isRoot(source) // enforce the source node to be a root node
    xmlDom.wrapInRoot(transform(sheet, xmlDom.createSingletonList(rootSource), Map(), Map()))
  }

  /** Transforms a list of source nodes to a new list of nodes using given variable and parameter bindings */
  private def transform(sheet: XSLTStylesheet, sources: L, variables: Map[String, V], params: Map[String, V]): L = {
    // create context, choose template, instantiate template, append results
    xmlDom.flatMapWithIndex(sources, (node, index) => {
      val templates = chooseTemplates(sheet, node)

      xmlDom.joinAllLists(templates.map { case (tmpl, specificNode) =>
        val context = AbstractXSLTContext[N, L, V](specificNode, sources, xpathDom.add(index, xpathDom.liftNumber(1)), variables)
        instantiateTemplate(sheet, tmpl, context, params)
      })
    })
  }

  /** Instantiates an XSLT template in a given abstract XSLT context with parameters and returns a list of resulting nodes.
    *
    * @param tmpl the template to instantiate
    * @param context the context used to instantiate the template
    * @param params the parameters to use for instantiating the template (they will be ignored if they don't have
    *               a corresponding default parameter in the template, see XSLT spec section 11.6)
    * @return a list of resulting XML nodes
    */
  private def instantiateTemplate(sheet: XSLTStylesheet, tmpl: XSLTTemplate, context: AbstractXSLTContext[N, L, V], params: Map[String, V]): L = {
    val acceptedParams = params.filter { case (key, _) => tmpl.defaultParams.contains(key) }
    val remainingDefaultParams = tmpl.defaultParams.filter { case (key, _) => !params.contains(key)}.mapValues(v => xpathAnalyzer.process(v, xsltToXPathContext(context)))
    // the context for the newly instantiated template contains only global variables and parameters, no local parameters
    // (static scoping and no nested template definitions); global variables are not supported in this implementation
    process(sheet, tmpl.content, context.replaceVariables(Map()).addVariables(remainingDefaultParams ++ acceptedParams))
  }

  private def chooseTemplates(sheet: XSLTStylesheet, node: N): Map[XSLTTemplate, N] = {
    val result = scala.collection.mutable.Map[XSLTTemplate, N]()
    var currentNode = node
    breakable {
      // iterate through matchable templates in reverse order (because always the last one always has highest priority/precedence)
      sheet.matchableTemplates.reverse.foreach { case (path, tpl, _, _) =>
        val (matches, notMatches) = xpathMatcher.matches(currentNode, path)
        if (!xmlDom.lessThanOrEqual(matches, xmlDom.bottom))
          result.put(tpl, matches) // template might match, so add it to possible results
        if (xmlDom.lessThanOrEqual(notMatches, xmlDom.bottom))
          break() // there is nothing left did not match, so we can stop the process
        if (xmlDom.lessThanOrEqual(currentNode, matches))
          break() // the node matched completely, so we can also stop

        currentNode = notMatches // continue with that "part" of the node that did not match
        // (we already found the correct template for everything that did match so far)
      }
    }
    result.toMap
  }

  /** Processes a sequence of XSLT instructions using a new scope (variable definitions are collected
    * so they will be visible in subsequent nodes, but never outside of the scope)
    *
    * @param nodes the instructions to process
    * @param context the context to evaluate the first instruction in (subsequent instructions might have additional variable bindings)
    * @return a list of resulting XML nodes
    */
  private def process(sheet: XSLTStylesheet, nodes: Seq[XSLTInstruction], context: AbstractXSLTContext[N, L, V]): L = {
    // remember variable names that were created in this scope so we can throw an error
    // if any of these is shadowed in the SAME scope
    var scopeVariables = scala.collection.mutable.Set[String]()

    val (result, _) = nodes.foldLeft((xmlDom.createEmptyList(), context)) {
      case ((resultNodes, ctx), next) =>
        process(sheet, next, ctx) match {
          case Left(moreResultNodes) => (xmlDom.concatLists(resultNodes, moreResultNodes), ctx)
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
  private def process(sheet: XSLTStylesheet, node: XSLTInstruction, context: AbstractXSLTContext[N, L, V]): Either[L, (String, V)] = {
    node match {
      case CreateElementInstruction(name, children) =>
        val innerNodes = process(sheet, children, context)
        val (resultAttributes, resultChildren) = xmlDom.partitionAttributes(innerNodes)
        val result = xmlDom.createElement(name, resultAttributes, resultChildren)
        Left(xmlDom.createSingletonList(result))
      case CreateTextInstruction(text) =>
        if (text == "")
          Left(xmlDom.createEmptyList())
        else
          Left(xmlDom.createSingletonList(xmlDom.createTextNode(xpathDom.liftString(text))))
      case CreateCommentInstruction(value) =>
        // merge the content of all text-node children to create the attribute value
        val textResult = xmlDom.getConcatenatedTextNodeValues(process(sheet, value, context))
        Left(xmlDom.createSingletonList(xmlDom.createComment(textResult)))
      case SetAttributeInstruction(attribute, value) =>
        // merge the content of all text-node children to create the attribute value
        val textResult = xmlDom.getConcatenatedTextNodeValues(process(sheet, value, context))
        Left(xmlDom.createSingletonList(xmlDom.createAttribute(attribute, textResult)))
      case ApplyTemplatesInstruction(None, params) =>
        Left(transform(sheet, xmlDom.getChildren(context.node), context.variables, params.mapValues(v => xpathAnalyzer.process(v, xsltToXPathContext(context)))))
      case ApplyTemplatesInstruction(Some(expr), params) =>
        val result = xpathAnalyzer.process(expr, xsltToXPathContext(context))
        val (extracted, _) = xpathDom.matchNodeSetValues(result)
        Left(transform(sheet, extracted, context.variables, params.mapValues(v => xpathAnalyzer.process(v, xsltToXPathContext(context)))))
      case CallTemplatesInstruction(name, params) =>
        // unlike apply-templates, call-template does not change the current node or current node list (see spec section 6)
        Left(instantiateTemplate(sheet, sheet.namedTemplates(name), context, params.mapValues(v => xpathAnalyzer.process(v, xsltToXPathContext(context)))))
      case VariableDefinitionInstruction(name, expr) =>
        Right(name, xpathAnalyzer.process(expr, xsltToXPathContext(context)))
      case CopyInstruction(select) =>
        val result = xpathAnalyzer.process(select, xsltToXPathContext(context))
        val (nodeSets, rest) = xpathDom.matchNodeSetValues(result)
        val nodeSetsOutput = xmlDom.copyToOutput(nodeSets)
        val restAsString = xpathDom.toStringValue(rest)
        var restOutput = xmlDom.createSingletonList(xmlDom.createTextNode(restAsString))
        if (xpathDom.lessThanOrEqual(xpathDom.liftString(""), restAsString)) {
          // if the empty string is a possible value, include the empty list in the result
          // (because empty strings don't create any text node and therefore create an empty result list)
          restOutput = xmlDom.joinLists(restOutput, xmlDom.createEmptyList())
        }
        Left(xmlDom.joinLists(nodeSetsOutput, restOutput))
      case ChooseInstruction(branches, otherwise) =>
        val possibleBranches = chooseBranches(branches, otherwise, xsltToXPathContext(context))
        // evaluate all possible branches and join the result lists
        Left(xmlDom.joinAllLists(possibleBranches.map(br => process(sheet, br, context))))

    }
  }

  /** Chooses the possible branches of a choose instruction.
    *
    * @param branches the remaining branches to test
    * @param otherwise the otherwise branch that will be evaluated when there is no other branch left
    * @param context the context to evaluate the instructions in
    * @return a set of possible branches, represented by their body as XSLT instructions
    */
  private def chooseBranches(branches: List[(XPathExpr, Seq[XSLTInstruction])], otherwise: Seq[XSLTInstruction], context: AbstractXPathContext[N, L, V]): Set[Seq[XSLTInstruction]] = {
    branches match {
      case Nil => Set(otherwise)
      case (firstExpr, firstTmpl) :: rest =>
        val result = xpathDom.toBooleanValue(xpathAnalyzer.process(firstExpr, context))
        val maybeTrue = xpathDom.lessThanOrEqual(xpathDom.liftBoolean(true), result)
        val maybeFalse = xpathDom.lessThanOrEqual(xpathDom.liftBoolean(false), result)
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
