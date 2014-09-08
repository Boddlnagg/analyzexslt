package xslt

import xml.XMLNode
import xpath.{XPathContext,XPathValue}

/** A context for evaluating an XSLT template (see XSLT spec section 1)
  *
  * @param node the current node
  * @param nodeList the current node list
  * @param position the current node's position in the current node list
  * @param variables a set of variable bindings
  */
case class XSLTContext(node: XMLNode, nodeList: List[XMLNode], position: Int, variables: Map[String, XPathValue]) {
  /** Creates an equivalent XPath context from this XSLT context */
  def toXPathContext = XPathContext(node, position, nodeList.size, variables)

  /** Returns a new context where a variable binding was added to the set of variables */
  def addVariable(name: String, value: XPathValue): XSLTContext = {
    XSLTContext(node, nodeList, position, variables + (name -> value))
  }

  /** Returns a new context where a number of variable bindings were added to the set of variables */
  def addVariables(newVariables: Map[String, XPathValue]): XSLTContext = {
    XSLTContext(node, nodeList, position, variables ++ newVariables)
  }

  /** Returns a new context where the variable bindings were replaced by the given new set of variable */
  def replaceVariables(newVariables: Map[String, XPathValue]): XSLTContext = {
    XSLTContext(node, nodeList, position, newVariables)
  }
}
