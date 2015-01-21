package analysis

import xpath.XPathExpr
import xslt._
import util.ProcessingError
import analysis.domain.Domain
import scala.util.control.Breaks._
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

/** Class to analyze XSLT stylesheets using abstract interpretation */
class XSLTAnalyzer[N, L, V](dom: Domain[N, L, V]) {

  val xmlDom = dom.xmlDom
  val xpathDom = dom.xpathDom
  val xpathAnalyzer = new XPathAnalyzer[N, L, V](dom)
  val patternMatcher = new AbstractPatternMatcher[N, L, V](xmlDom)

  /** Transforms a source document (represented by its root node) into a new document using an XSLT stylesheet */
  def transform(sheet: XSLTStylesheet, source: N): N = {
    val (rootSource, _) = xmlDom.isRoot(source) // enforce the source node to be a root node
    xmlDom.wrapInRoot(applyTemplates(sheet, xmlDom.createSingletonList(rootSource), None, Map(), Map()))
  }

  /** Applies matching templates to a list of given source nodes and produces a new list of nodes */
  private def applyTemplates(sheet: XSLTStylesheet, sources: L, mode: Option[String], variables: Map[String, V], params: Map[String, V]): L = {
    xmlDom.flatMapWithIndex(sources, (node, index) => {
      val templates = chooseTemplates(sheet, node, mode)
      xmlDom.joinAllLists(templates.map { case (tmpl, specificNode) =>
        val context = AbstractXSLTContext[N, L, V](specificNode, sources, xpathDom.add(index, xpathDom.liftNumber(1)), variables)
        instantiateTemplate(sheet, tmpl, context, params)
      })
    })
  }

  /** Instantiates an XSLT template in a given abstract XSLT context with parameters and returns a list of resulting nodes.
    *
    * @param sheet the stylesheet that is currently processed
    * @param tmpl the template to instantiate
    * @param context the context used to instantiate the template
    * @param params the parameters to use for instantiating the template (they will be ignored if they don't have
    *               a corresponding default parameter in the template, see XSLT spec section 11.6)
    * @return a list of resulting XML nodes
    */
  private def instantiateTemplate(sheet: XSLTStylesheet, tmpl: XSLTTemplate, context: AbstractXSLTContext[N, L, V], params: Map[String, V]): L = {
    val acceptedParams = params.filter { case (key, _) => tmpl.defaultParams.contains(key) }
    val remainingDefaultParams = tmpl.defaultParams.filter { case (key, _) => !params.contains(key) }
      .mapValues(v => xpathAnalyzer.evaluate(v, xsltToXPathContext(context)))
    // the context for the newly instantiated template contains only global variables and parameters,
    // no local parameters (static scoping and no nested template definitions);
    // global variables are not supported in this implementation
    processAll(sheet, tmpl.content, context.replaceVariables(remainingDefaultParams ++ acceptedParams))
  }

  private def chooseTemplates(sheet: XSLTStylesheet, node: N, mode: Option[String]): Map[XSLTTemplate, N] = {
    val result = MutableMap[XSLTTemplate, N]()
    val matchable = sheet.matchableTemplates.getOrElse(mode, Map())
    var currentNode = node
    breakable {
      // iterate through matchable templates (they are ordered s.t. the first one always has highest priority/precedence)
      matchable.foreach { case (path, tpl) =>
        val (matches, notMatches) = patternMatcher.matches(currentNode, path)
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
    * @param sheet the stylesheet that is currently processed
    * @param instructions the instructions to process
    * @param context the context to evaluate the first instruction in (subsequent instructions might have additional variable bindings)
    * @return a list of resulting XML nodes
    */
  private def processAll(sheet: XSLTStylesheet, instructions: Seq[XSLTInstruction], context: AbstractXSLTContext[N, L, V]): L = {
    // remember variable names that were created in this scope so we can throw an error
    // if any of these is shadowed in the SAME scope
    var scopeVariables = MutableSet[String]()

    val (result, _) = instructions.foldLeft((xmlDom.createEmptyList(), context)) {
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
    *
    * @param sheet the stylesheet that is currently processed
    * @param instruction the instruction to process
    * @param context the context to evaluate the instruction in
    * @return either a list of resulting XML nodes or a new variable binding
    */
  private def process(sheet: XSLTStylesheet, instruction: XSLTInstruction, context: AbstractXSLTContext[N, L, V]): Either[L, (String, V)] = {
    instruction match {
      case CreateElementInstruction(name, content) =>
        val innerNodes = processAll(sheet, content, context)
        val (attributes, children) = xmlDom.partitionAttributes(innerNodes)
        val evaluatedName = xpathDom.concatAllStrings(name.map {
          case Left(str) => xpathDom.liftString(str)
          case Right(expr) => xpathDom.toStringValue(xpathAnalyzer.evaluate(expr, xsltToXPathContext(context)))
        })
        val result = xmlDom.createElement(evaluatedName, attributes, children)
        Left(xmlDom.createSingletonList(result))
      case CreateTextInstruction(text) =>
        if (text == "")
          Left(xmlDom.createEmptyList())
        else
          Left(xmlDom.createSingletonList(xmlDom.createTextNode(xpathDom.liftString(text))))
      case CreateCommentInstruction(value) =>
        // merge the content of all text-node children to create the attribute value
        val textResult = xmlDom.getConcatenatedTextNodeValues(processAll(sheet, value, context))
        Left(xmlDom.createSingletonList(xmlDom.createComment(textResult)))
      case SetAttributeInstruction(name, value) =>
        // merge the content of all text-node children to create the attribute value
        val textResult = xmlDom.getConcatenatedTextNodeValues(processAll(sheet, value, context))
        val evaluatedName = xpathDom.concatAllStrings(name.map {
          case Left(str) => xpathDom.liftString(str)
          case Right(expr) => xpathDom.toStringValue(xpathAnalyzer.evaluate(expr, xsltToXPathContext(context)))
        })
        Left(xmlDom.createSingletonList(xmlDom.createAttribute(evaluatedName, textResult)))
      case ApplyTemplatesInstruction(None, mode, params) =>
        Left(applyTemplates(sheet, xmlDom.getChildren(context.node), mode, context.variables, params.mapValues(v => xpathAnalyzer.evaluate(v, xsltToXPathContext(context)))))
      case ApplyTemplatesInstruction(Some(expr), mode, params) =>
        val result = xpathAnalyzer.evaluate(expr, xsltToXPathContext(context))
        val (extracted, _) = xpathDom.matchNodeSetValues(result)
        Left(applyTemplates(sheet, extracted, mode, context.variables, params.mapValues(v => xpathAnalyzer.evaluate(v, xsltToXPathContext(context)))))
      case CallTemplateInstruction(name, params) =>
        // unlike apply-templates, call-template does not change the current node or current node list (see spec section 6)
        Left(instantiateTemplate(sheet, sheet.namedTemplates(name), context, params.mapValues(v => xpathAnalyzer.evaluate(v, xsltToXPathContext(context)))))
      case VariableDefinitionInstruction(name, expr) =>
        Right(name, xpathAnalyzer.evaluate(expr, xsltToXPathContext(context)))
      case CopyInstruction(content) =>
        val (root, notRoot) = xmlDom.isRoot(context.node)
        var result = xmlDom.bottomList
        if (!xmlDom.lessThanOrEqual(root, xmlDom.bottom)) { // root is not BOTTOM -> node might be a root node
          result = xmlDom.joinLists(result, processAll(sheet, content, context))
        }
        val (elem, notElem) = xmlDom.isElement(notRoot)
        if (!xmlDom.lessThanOrEqual(elem, xmlDom.bottom)) { // elem is not BOTTOM -> node might be an element node
          val innerNodes = processAll(sheet, content, context)
          val (resultAttributes, resultChildren) = xmlDom.partitionAttributes(innerNodes)
          val elemResult = xmlDom.createElement(xmlDom.getNodeName(elem), resultAttributes, resultChildren)
          result = xmlDom.joinLists(result, xmlDom.createSingletonList(elemResult))
        }
        if (!xmlDom.lessThanOrEqual(notElem, xmlDom.bottom)) {
          // notElem is not BOTTOM -> there are nodes that are neither elements nor root nodes
          // those can be copied directly
          result = xmlDom.joinLists(result, xmlDom.copyToOutput(xmlDom.createSingletonList(notElem)))
        }
        Left(result)
      case CopyOfInstruction(select) =>
        val result = xpathAnalyzer.evaluate(select, xsltToXPathContext(context))
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
        Left(xmlDom.joinAllLists(possibleBranches.map(br => processAll(sheet, br, context))))
      case ForEachInstruction(select, content) =>
        val result = xpathAnalyzer.evaluate(select, xsltToXPathContext(context))
        val (extracted, _) = xpathDom.matchNodeSetValues(result)
        Left(xmlDom.flatMapWithIndex(extracted, (n, i) => {
          val newContext = AbstractXSLTContext(n, extracted, xpathDom.add(i, xpathDom.liftNumber(1)), context.variables)
          processAll(sheet, content, newContext)
        }))
      case NumberInstruction() =>
        // NOTE: this is a dummy implementation
        Left(xmlDom.createSingletonList(xmlDom.createTextNode(xpathDom.topString)))
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
        val result = xpathDom.toBooleanValue(xpathAnalyzer.evaluate(firstExpr, context))
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
