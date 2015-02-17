package analysis

import xpath.{XPathValue, XPathExpr}
import xslt._
import util.ProcessingError
import analysis.domain.Domain
import scala.util.control.Breaks._
import scala.collection.mutable.{MutableList => MutList, Set => MutSet}

/** Class to analyze XSLT stylesheets using abstract interpretation */
class XSLTAnalyzer[N, L, V](dom: Domain[N, L, V]) {

  val xmlDom = dom.xmlDom
  val xpathDom = dom.xpathDom
  val xpathAnalyzer = new XPathAnalyzer[N, L, V](dom)
  val patternMatcher = new AbstractPatternMatcher[N, L, V](xmlDom)

  /** Transforms a source document (represented by its root node) into a new document using an XSLT stylesheet */
  def transform(sheet: XSLTStylesheet, source: N, recursionLimit: Option[Int] = None): N = {
    val (rootSource, _) = xmlDom.isRoot(source, allowResultTreeFragments = false) // enforce the source node to be a (non-fragment) root node

    val globalVariables = evaluateVariables(sheet, sheet.globalVariables, AbstractXSLTContext(rootSource, xmlDom.createSingletonList(rootSource), xpathDom.liftNumber(1), Map(), Map()), recursionLimit)
    xmlDom.createRoot(applyTemplates(sheet, xmlDom.createSingletonList(rootSource), None, globalVariables, Map(), Map(), recursionLimit), isResultTreeFragment = false)
  }

  /** Applies matching templates to a list of given source nodes and produces a new list of nodes */
  private def applyTemplates(sheet: XSLTStylesheet, sources: L, mode: Option[String], globalVariables: Map[String, V], localVariables: Map[String, V], params: Map[String, V], recursionLimit: Option[Int]): L = {
    xmlDom.flatMapWithIndex(sources, (node, index) => {
      val templates = chooseTemplates(sheet, node, mode)
      // use templates.reverse.view as performance optimization:
      // - reverse list, so most general template comes first
      // - and when this evaluates to TOP, the remaining templates are not even looked at
      //   because joinAllLists returns early
      joinAllLists(templates.reverse.view.map { case (tmpl, specificNode) =>
        val context = AbstractXSLTContext[N, L, V](specificNode, sources, xpathDom.add(index, xpathDom.liftNumber(1)), globalVariables, localVariables)
        instantiateTemplate(sheet, tmpl, context, params, recursionLimit)
      })
    })
  }

  private def joinAllLists(lists: Iterable[L]): L = lists.fold(xmlDom.bottomList) { (l1, l2) =>
    if (xmlDom.lessThanOrEqualLists(xmlDom.topList, l1)) return xmlDom.topList
    else xmlDom.joinLists(l1, l2) // continue only if l1 < TOP
  }

  private def chooseTemplates(sheet: XSLTStylesheet, node: N, mode: Option[String]): List[(XSLTTemplate, N)] = {
    val result = MutList[(XSLTTemplate, N)]()
    val matchable = sheet.matchableTemplates.getOrElse(mode, Map())
    var currentNode = node
    breakable {
      // iterate through matchable templates (they are ordered s.t. the first one always has highest priority/precedence)
      matchable.foreach { case (path, tpl) =>
        val (matches, notMatches) = patternMatcher.matches(currentNode, path)
        if (!xmlDom.lessThanOrEqual(matches, xmlDom.bottom))
          result += ((tpl, matches)) // template might match, so add it to possible results
        if (xmlDom.lessThanOrEqual(notMatches, xmlDom.bottom) || xmlDom.lessThanOrEqual(currentNode, matches))
          break() // the node matched completely, so we can stop the process

        currentNode = notMatches // continue with that "part" of the node that did not match
        // (we already found the correct template for everything that did match so far)
      }
    }
    result.toList
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
  private def instantiateTemplate(sheet: XSLTStylesheet, tmpl: XSLTTemplate, context: AbstractXSLTContext[N, L, V], params: Map[String, V], recursionLimit: Option[Int]): L = {
    if (recursionLimit == Some(0)) return xmlDom.topList

    val evaluatedParams = tmpl.defaultParams.foldLeft(Map[String, V]()) {
      case (current, (name, _)) if params.contains(name) => current + (name -> params(name))
      case (current, (name, value)) => current + (name -> evaluateVariable(sheet, value, context.addLocalVariables(current), recursionLimit))
    }
    // the context for the newly instantiated template contains only global variables and parameters,
    // no other local variables (static scoping and no nested template definitions)
    processAll(sheet, tmpl.content, context.replaceLocalVariables(evaluatedParams), recursionLimit.map(_ - 1))
  }

  /** Processes a sequence of XSLT instructions using a new scope (variable definitions are collected
    * so they will be visible in subsequent nodes, but never outside of the scope)
    *
    * @param sheet the stylesheet that is currently processed
    * @param instructions the instructions to process
    * @param context the context to evaluate the first instruction in (subsequent instructions might have additional variable bindings)
    * @return a list of resulting XML nodes
    */
  private def processAll(sheet: XSLTStylesheet, instructions: Seq[XSLTInstruction], context: AbstractXSLTContext[N, L, V], recursionLimit: Option[Int]): L = {
    // remember variable names that were created in this scope so we can throw an error
    // if any of these is shadowed in the SAME scope
    var scopeVariables = MutSet[String]()

    val (result, _) = instructions.foldLeft((xmlDom.createEmptyList(), context)) {
      case ((resultNodes, ctx), next) =>
        process(sheet, next, ctx, recursionLimit) match {
          case Left(moreResultNodes) => (xmlDom.concatLists(resultNodes, moreResultNodes), ctx)
          case Right((name, value)) =>
            if (scopeVariables.contains(name)) throw new ProcessingError(f"Variable $name is defined multiple times in the same scope")
            scopeVariables += name
            (resultNodes, ctx.addLocalVariable(name, value))
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
  private def process(sheet: XSLTStylesheet, instruction: XSLTInstruction, context: AbstractXSLTContext[N, L, V], recursionLimit: Option[Int]): Either[L, (String, V)] = {
    instruction match {
      case CreateElementInstruction(name, content) =>
        val innerNodes = processAll(sheet, content, context, recursionLimit)
        val attributes = xmlDom.takeWhile(innerNodes, n => xmlDom.isAttribute(n))
        val children = xmlDom.filter(innerNodes, n => {
          val (isAttr, isNotAttr) = xmlDom.isAttribute(n)
          (isNotAttr, isAttr) // swap positive and negative results of isAttribute
        })
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
        val textResult = xmlDom.getConcatenatedTextNodeValues(processAll(sheet, value, context, recursionLimit))
        Left(xmlDom.createSingletonList(xmlDom.createComment(textResult)))
      case SetAttributeInstruction(name, value) =>
        // merge the content of all text-node children to create the attribute value
        val textResult = xmlDom.getConcatenatedTextNodeValues(processAll(sheet, value, context, recursionLimit))
        val evaluatedName = xpathDom.concatAllStrings(name.map {
          case Left(str) => xpathDom.liftString(str)
          case Right(expr) => xpathDom.toStringValue(xpathAnalyzer.evaluate(expr, xsltToXPathContext(context)))
        })
        Left(xmlDom.createSingletonList(xmlDom.createAttribute(evaluatedName, textResult)))
      case ApplyTemplatesInstruction(None, mode, params) =>
        Left(applyTemplates(sheet, xmlDom.getChildren(context.node), mode, context.globalVariables, context.localVariables, params.mapValues(v => evaluateVariable(sheet, v, context, recursionLimit)), recursionLimit))
      case ApplyTemplatesInstruction(Some(expr), mode, params) =>
        val result = xpathAnalyzer.evaluate(expr, xsltToXPathContext(context))
        val (extracted, _) = xpathDom.matchNodeSet(result)
        Left(applyTemplates(sheet, extracted, mode, context.globalVariables, context.localVariables, params.mapValues(v => evaluateVariable(sheet, v, context, recursionLimit)), recursionLimit))
      case CallTemplateInstruction(name, params) =>
        // unlike apply-templates, call-template does not change the current node or current node list (see spec section 6)
        Left(instantiateTemplate(sheet, sheet.namedTemplates(name), context, params.mapValues(v => evaluateVariable(sheet, v, context, recursionLimit)), recursionLimit))
      case VariableDefinitionInstruction(name, value) =>
        Right(name, evaluateVariable(sheet, value, context, recursionLimit))
      case CopyInstruction(content) =>
        val (root, notRoot) = xmlDom.isRoot(context.node)
        var result = xmlDom.bottomList
        if (!xmlDom.lessThanOrEqual(root, xmlDom.bottom)) { // root is not BOTTOM -> node might be a root node
          result = xmlDom.joinLists(result, processAll(sheet, content, context, recursionLimit))
        }
        val (elem, notElem) = xmlDom.isElement(notRoot)
        if (!xmlDom.lessThanOrEqual(elem, xmlDom.bottom)) { // elem is not BOTTOM -> node might be an element node
          val innerNodes = processAll(sheet, content, context, recursionLimit)
          val resultAttributes = xmlDom.takeWhile(innerNodes, n => xmlDom.isAttribute(n))
          val resultChildren = xmlDom.filter(innerNodes, n => {
            val (isAttr, isNotAttr) = xmlDom.isAttribute(n)
            (isNotAttr, isAttr) // swap positive and negative results of isAttribute
          })
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
        val (nodeSets, rest) = xpathDom.matchNodeSet(result)
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
        Left(possibleBranches.map(br => processAll(sheet, br, context, recursionLimit)).fold(xmlDom.bottomList)(xmlDom.joinLists))
      case ForEachInstruction(select, content) =>
        val result = xpathAnalyzer.evaluate(select, xsltToXPathContext(context))
        val (extracted, _) = xpathDom.matchNodeSet(result)
        Left(xmlDom.flatMapWithIndex(extracted, (n, i) => {
          val newContext = AbstractXSLTContext(n, extracted, xpathDom.add(i, xpathDom.liftNumber(1)), context.globalVariables, context.localVariables)
          processAll(sheet, content, newContext, recursionLimit)
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

  /** Evaluates a list of variable definitions in the order they are defined. */
  def evaluateVariables(sheet: XSLTStylesheet, variables: List[(String, Either[XPathExpr, Seq[XSLTInstruction]])], context: AbstractXSLTContext[N, L, V], recursionLimit: Option[Int]): Map[String, V] = {
    variables.foldLeft(Map[String, V]()) {
      case (current, (name, value)) => current + (name -> evaluateVariable(sheet, value, context.addLocalVariables(current), recursionLimit))
    }
  }

  /** Evaluates a single variable value. */
  def evaluateVariable(sheet: XSLTStylesheet, variable: Either[XPathExpr, Seq[XSLTInstruction]], context: AbstractXSLTContext[N, L, V], recursionLimit: Option[Int]): V = variable match {
    case Left(expr) =>
      xpathAnalyzer.evaluate(expr, xsltToXPathContext(context))
    case Right(instructions) =>
      xpathDom.createNodeSet(xmlDom.createSingletonList(xmlDom.createRoot(processAll(sheet, instructions, context, recursionLimit), isResultTreeFragment = true)))
  }
}
