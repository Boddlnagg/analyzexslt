package analysis

import analysis.domain.{XPathDomain, XMLDomain}

case class AbstractXSLTContext[N, D1 <: XMLDomain[N], T, D2 <: XPathDomain[T]](node: N, nodeListSize: Option[Int], position: Option[Int], variables: Map[String, T]) {
  /** Creates an equivalent XPath context from this XSLT context */
  def toXPathContext = AbstractXPathContext(node, position, nodeListSize, variables)

  /** Returns a new context where a variable binding was added to the set of variables */
  def addVariable(name: String, value: T): AbstractXSLTContext[N, D1, T, D2] = {
    AbstractXSLTContext(node, nodeListSize, position, variables + (name -> value))
  }

  /** Returns a new context where a number of variable bindings were added to the set of variables */
  def addVariables(newVariables: Map[String, T]): AbstractXSLTContext[N, D1, T, D2] = {
    AbstractXSLTContext(node, nodeListSize, position, variables ++ newVariables)
  }

  /** Returns a new context where the variable bindings were replaced by the given new set of variable */
  def replaceVariables(newVariables: Map[String, T]): AbstractXSLTContext[N, D1, T, D2] = {
    AbstractXSLTContext(node, nodeListSize, position, newVariables)
  }
}
