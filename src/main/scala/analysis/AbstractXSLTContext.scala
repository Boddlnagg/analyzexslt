package analysis

import analysis.domain.{XPathDomain, XMLDomain}

/** A context for evaluating an XSLT template (see XSLT spec section 1)
  *
  * @param node the current node
  * @param nodeList the current node list
  * @param position the current node's position in the current node list
  * @param variables a set of variable bindings
  */
case class AbstractXSLTContext[N, D1 <: XMLDomain[N], T, D2 <: XPathDomain[T]](node: N, nodeList: List[N], position: Int, variables: Map[String, T]) {
  /** Creates an equivalent XPath context from this XSLT context */
  //def toXPathContext = XPathContext(node, position, nodeList.size, variables)

  /** Returns a new context where a variable binding was added to the set of variables */
  def addVariable(name: String, value: T): AbstractXSLTContext[N, D1, T, D2] = {
    AbstractXSLTContext(node, nodeList, position, variables + (name -> value))
  }

  /** Returns a new context where a number of variable bindings were added to the set of variables */
  def addVariables(newVariables: Map[String, T]): AbstractXSLTContext[N, D1, T, D2] = {
    AbstractXSLTContext(node, nodeList, position, variables ++ newVariables)
  }

  /** Returns a new context where the variable bindings were replaced by the given new set of variable */
  def replaceVariables(newVariables: Map[String, T]): AbstractXSLTContext[N, D1, T, D2] = {
    AbstractXSLTContext(node, nodeList, position, newVariables)
  }
}
