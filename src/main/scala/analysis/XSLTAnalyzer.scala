package analysis

import xslt._
import util.EvaluationError
import analysis.domain.{XMLDomain, XPathDomain}

/** Trait to analyze XSLT stylesheets using abstract interpretation */
trait XSLTAnalyzer[N, L, D1 <: XMLDomain[N, L], T, D2 <: XPathDomain[T, N, L, D1]] {

  val dom1: D1
  val dom2: D2

  /** Transforms a source document (represented by it's root node) into a new document using an XSLT stylesheet*/
  /*def transform(sheet: XSLTStylesheet, source: XMLRoot): XMLRoot = {
    // process according to XSLT spec section 5.1
    transform(sheet, List(source), Map(), Map()) match {
      case List(elem@XMLElement(_, _, _, _)) => XMLRoot(elem)
      case _ => throw new IllegalStateException("Transformation result must be a single XMLElement")
    }
  }*/

  /** Transforms a list of source nodes to a new list of nodes using given variable and parameter bindings */
  def transform(sheet: XSLTStylesheet, sources: L, variables: Map[String, T], params: Map[String, T]): L = {
    // TODO: sources probably can't be List[N], because we don't always have a list (needs to be more abstract)
    // create context, choose template, instantiate template, append results
    val x = sources.zipWithIndex
      .map { case (n,i) => (dom1.chooseTemplates(sheet, n), AbstractXSLTContext(n, Some(sources.size), Some(i + 1), variables)) }
      //.flatMap { case (tmpl, context) => evaluateTemplate(sheet, tmpl, context, params) }
    null
  }

  /** Evaluates an XSLT template in a given XSLT context with parameters and returns a list of resulting nodes.
    *
    * @param tmpl the template to evaluate
    * @param context the context to evaluate the template in
    * @param params the parameters to use for evaluating the template (they will be ignored if they don't have
    *               a corresponding default parameter in the template, see XSLT spec section 11.6)
    * @return a list of resulting XML nodes
    */
  /*def evaluateTemplate(sheet: XSLTStylesheet, tmpl: XSLTTemplate, context: XSLTContext, params: Map[String, T]): List[N] = {
    val acceptedParams = params.filter { case (key, _) => tmpl.defaultParams.contains(key) }
    val remainingDefaultParams = tmpl.defaultParams.filter { case (key, _) => !params.contains(key)}.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))
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
  def evaluate(sheet: XSLTStylesheet, nodes: Seq[XSLTInstruction], context: XSLTContext): List[N] = {
    // remember variable names that were created in this scope so we can throw an error
    // if any of these is shadowed in the SAME scope
    var scopeVariables = scala.collection.mutable.Set[String]()

    val (result, _) = nodes.foldLeft((List[N](), context)) {
      case ((resultNodes, ctx), next) =>
        evaluate(sheet, next, ctx) match {
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
  def evaluate(sheet: XSLTStylesheet, node: XSLTInstruction, context: XSLTContext): Either[List[N], (String, T)] = {
    node match {
      case LiteralElement(name, attributes, children) =>
        val resultNodes = evaluate(sheet, children, context)
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
        val textResult = evaluate(sheet, value, context)
          .collect { case n: XMLTextNode => n.value }
          .mkString("")
        Left(List(XMLAttribute(attribute, textResult)))
      case ApplyTemplatesInstruction(None, params) =>
        context.node match {
          case root: XMLRoot => Left(transform(sheet, List(root.elem), context.variables, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
          case elem: XMLElement => Left(transform(sheet, elem.children.toList, context.variables, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
          case _ => Left(Nil) // other node types don't have children and return an empty result
        }
      case ApplyTemplatesInstruction(Some(expr), params) =>
        XPathEvaluator.evaluate(expr, context.toXPathContext) match {
          case NodeSetValue(nodes) => Left(transform(sheet, nodes, context.variables, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
          case value => throw new EvaluationError(f"select expression in apply-templates must evaluate to a node-set (evaluated to $value)")
        }
      case CallTemplatesInstruction(name, params) =>
        // unlike apply-templates, call-template does not change the current node or current node list (see spec section 6)
        Left(evaluateTemplate(sheet, sheet.namedTemplates(name), context, params.mapValues(v => XPathEvaluator.evaluate(v, context.toXPathContext))))
      case VariableDefinitionInstruction(name, expr) =>
        Right(name, XPathEvaluator.evaluate(expr, context.toXPathContext))
      case CopyInstruction(select) =>
        XPathEvaluator.evaluate(select, context.toXPathContext) match {
          // NOTE: result tree fragments are generally not supported
          case NodeSetValue(nodes) => Left(nodes.map {
            case XMLRoot(elem) => elem.copy // "a root node is copied by copying its children" according to spec
            case node => node.copy
          })
          case value => Left(List(XMLTextNode(value.toStringValue.value)))
        }
      case ChooseInstruction(branches, otherwise) =>
        Left(evaluate(sheet, evaluateChoose(branches, otherwise, context.toXPathContext), context))
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
  }*/
}
