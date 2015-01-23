package xslt

import util.ProcessingError
import xml._
import xpath._

import scala.collection.mutable.{Set => MutableSet}

/** Object to process XSLT stylesheets. */
object XSLTProcessor {
  /** Transforms a source document (represented by its root node) into a new document using an XSLT stylesheet */
  def transform(sheet: XSLTStylesheet, source: XMLRoot): XMLRoot = {
    // process according to XSLT spec section 5.1
    applyTemplates(sheet, List(source), None, Map(), Map()) match {
      case List(inner: XMLElement) => XMLRoot(List(inner)) // TODO: support comments here?
      case x => throw new ProcessingError("Transformation result must be a single XMLElement")
    }
  }

  /** Applies matching templates to a list of given source nodes and produces a new list of nodes */
  def applyTemplates(sheet: XSLTStylesheet, sources: List[XMLNode], mode: Option[String], variables: Map[String, XPathValue], params: Map[String, XPathValue]): List[XMLNode] = {
    sources.zipWithIndex.flatMap { case (n, i) =>
      val tmpl = chooseTemplate(sheet, n, mode)
      instantiateTemplate(sheet, tmpl, XSLTContext(n, sources, i + 1, variables), params)
    }
  }

  /** Chooses a template that matches the given element best */
  def chooseTemplate(sheet: XSLTStylesheet, node: XMLNode, mode: Option[String]): XSLTTemplate = {
    sheet.matchableTemplates(mode).find { case (path, _) => XSLTPatternMatcher.matches(node, path) } match {
      case Some((_, tmpl)) => tmpl
      case None => throw new ProcessingError(f"Found no matching template for input node `${XMLNode.formatPath(node)}` [NOTE: this can only happen when builtin templates are disabled]")
    }
  }

  /** Instantiates an XSLT template in a given XSLT context with parameters and returns a list of resulting nodes.
    *
    * @param sheet the stylesheet that is currently processed
    * @param tmpl the template to instantiate
    * @param context the context used to instantiate the template
    * @param params the parameters to use for instantiating the template (they will be ignored if they don't have
    *               a corresponding default parameter in the template, see XSLT spec section 11.6)
    * @return a list of resulting XML nodes
    */
  def instantiateTemplate(sheet: XSLTStylesheet, tmpl: XSLTTemplate, context: XSLTContext, params: Map[String, XPathValue]): List[XMLNode] = {
    val acceptedParams = params.filter { case (key, _) => tmpl.defaultParams.contains(key) }
    val remainingDefaultParams = tmpl.defaultParams.filter { case (key, _) => !params.contains(key) }
      .mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))
    // the context for the newly instantiated template contains only global variables and parameters,
    // no local parameters (static scoping and no nested template definitions);
    // global variables are not supported in this implementation
    processAll(sheet, tmpl.content, context.replaceVariables(remainingDefaultParams ++ acceptedParams))
  }

  /** Processes a sequence of XSLT instructions using a new scope (variable definitions are collected
    * so they will be visible in subsequent nodes, but never outside of the scope)
    *
    * @param sheet the stylesheet that is currently processed
    * @param instructions the instructions to process
    * @param context the context to evaluate the first instruction in (subsequent instructions might have additional variable bindings)
    * @return a list of resulting XML nodes
    */
  def processAll(sheet: XSLTStylesheet, instructions: Seq[XSLTInstruction], context: XSLTContext): List[XMLNode] = {
    // remember variable names that were created in this scope so we can throw an error
    // if any of these is shadowed in the SAME scope
    var scopeVariables = MutableSet[String]()

    val (result, _) = instructions.foldLeft((List[XMLNode](), context)) {
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
    *
    * @param sheet the stylesheet that is currently processed
    * @param instruction the instruction to process
    * @param context the context to evaluate the instruction in
    * @return either a list of resulting XML nodes or a new variable binding
    */
  def process(sheet: XSLTStylesheet, instruction: XSLTInstruction, context: XSLTContext): Either[List[XMLNode], (String, XPathValue)] = {
    instruction match {
      case CreateElementInstruction(name, content) =>
        val innerNodes = processAll(sheet, content, context)
        // attributes must come before all other result nodes, afterwards they are ignored (see spec section 7.1.3)
        val attributes = innerNodes.takeWhile(n => n.isInstanceOf[XMLAttribute]).map(n => n.asInstanceOf[XMLAttribute])
        val children = innerNodes.filter(n => !n.isInstanceOf[XMLAttribute])
        val evaluatedName = name.map {
          case Left(str) => str
          case Right(expr) => XPathEvaluator.evaluate(expr, context.toXPathContext).toStringValue.value
        }.mkString // concatenate literal and expression parts to get the element name
        Left(List(XMLElement(evaluatedName, attributes, children)))
      case CreateTextInstruction(text) =>
        if (text != "")
          Left(List(XMLTextNode(text)))
        else
          Left(Nil) // no text node will be created for the empty string
      case CreateCommentInstruction(value) =>
        // merge the content of all text-node children to create the comment value (non-text-node children are wrong and can be ignored according to spec)
        val textResult = processAll(sheet, value, context)
          .collect { case n: XMLTextNode => n.value }
          .mkString
        Left(List(XMLComment(textResult)))
      case SetAttributeInstruction(name, value) =>
        // merge the content of all text-node children to create the attribute value (non-text-node children are wrong and can be ignored according to spec)
        val textResult = processAll(sheet, value, context)
          .collect { case n: XMLTextNode => n.value }
          .mkString
        val evaluatedName = name.map {
          case Left(str) => str
          case Right(expr) => XPathEvaluator.evaluate(expr, context.toXPathContext).toStringValue.value
        }.mkString // concatenate literal and expression parts to get the attribute name
        Left(List(XMLAttribute(evaluatedName, textResult)))
      case ApplyTemplatesInstruction(None, mode, params) =>
        context.node match {
          case XMLRoot(children) => Left(applyTemplates(sheet, children, mode, context.variables, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
          case elem: XMLElement => Left(applyTemplates(sheet, elem.children.toList, mode, context.variables, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
          case _ => Left(Nil) // other node types don't have children and return an empty result
        }
      case ApplyTemplatesInstruction(Some(expr), mode, params) =>
        XPathEvaluator.evaluate(expr, context.toXPathContext) match {
          case NodeSetValue(nodes) => Left(applyTemplates(sheet, nodes.toList, mode, context.variables, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
          case value => throw new ProcessingError(f"select expression in apply-templates must evaluate to a node-set (evaluated to $value)")
        }
      case CallTemplateInstruction(name, params) =>
        // unlike apply-templates, call-template does not change the current node or current node list (see spec section 6)
        Left(instantiateTemplate(sheet, sheet.namedTemplates(name), context, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
      case VariableDefinitionInstruction(name, expr) =>
        Right(name, XPathEvaluator.evaluate(expr, context.toXPathContext))
      case CopyInstruction(content) =>
        context.node match {
          case XMLRoot(_) => Left(processAll(sheet, content, context))
          case XMLElement(name, _, _, _) =>
            val resultNodes = processAll(sheet, content, context)
            // attributes must come before all other result nodes, afterwards they are ignored (see spec section 7.1.3)
            val resultAttributes = resultNodes.takeWhile(n => n.isInstanceOf[XMLAttribute]).map(n => n.asInstanceOf[XMLAttribute])
            val resultChildren = resultNodes.filter(n => !n.isInstanceOf[XMLAttribute])
            Left(List(XMLElement(name, resultAttributes, resultChildren)))
          case n => Left(List(n.copy))
        }
      case CopyOfInstruction(select) =>
        XPathEvaluator.evaluate(select, context.toXPathContext) match {
          // NOTE: result tree fragments are generally not supported
          case NodeSetValue(nodes) => Left(nodes.toList.flatMap {
            case XMLRoot(children) => children.map(_.copy) // "a root node is copied by copying its children" according to spec
            case n => List(n.copy)
          })
          case other =>
            val textValue = other.toStringValue.value
            if (textValue != "")
              Left(List(XMLTextNode(other.toStringValue.value)))
            else
              Left(Nil) // text nodes with empty content are not allowed
        }
      case ChooseInstruction(branches, otherwise) =>
        Left(processAll(sheet, chooseBranch(branches, otherwise, context.toXPathContext), context))
      case ForEachInstruction(select, content) =>
        XPathEvaluator.evaluate(select, context.toXPathContext) match {
          case NodeSetValue(nodes) =>
            val nodeList = nodes.toList
            Left(nodeList.zipWithIndex.flatMap { case (n, i) =>
              val newContext = XSLTContext(n, nodeList, i + 1, context.variables)
              processAll(sheet, content, newContext)
            })
          case value => throw new ProcessingError(f"select expression in for-each must evaluate to a node-set (evaluated to $value)")
        }
      case NumberInstruction() => throw new NotImplementedError("Evaluation of the <xsl:number> instruction is not implemented.")
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
