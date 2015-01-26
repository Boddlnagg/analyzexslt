package xslt

import xml.XMLNode
import xpath.{XPathContext, XPathValue}

/** A context for evaluating an XSLT template (see XSLT spec section 1)
  *
  * @param node the current node
  * @param nodeList the current node list
  * @param position the current node's position in the current node list
  * @param globalVariables a set of global variable bindings
  * @param localVariables a set of local variable bindings
  */
case class XSLTContext(
  node: XMLNode,
  nodeList: List[XMLNode],
  position: Int,
  globalVariables: Map[String, XPathValue],
  localVariables: Map[String, XPathValue])
{
  val variables = globalVariables ++ localVariables

  /** Creates an equivalent XPath context from this XSLT context */
  def toXPathContext = XPathContext(node, position, nodeList.size, variables)

  /** Returns a new context where a local variable binding was added to the set of variables */
  def addLocalVariable(name: String, value: XPathValue): XSLTContext = {
    XSLTContext(node, nodeList, position, globalVariables, localVariables + (name -> value))
  }

  /** Returns a new context where a number of local variable bindings were added to the set of variables */
  def addLocalVariables(newVariables: Map[String, XPathValue]): XSLTContext = {
    XSLTContext(node, nodeList, position, globalVariables, localVariables ++ newVariables)
  }

  /** Returns a new context where the local variable bindings were replaced by the given new set of variable */
  def replaceLocalVariables(newVariables: Map[String, XPathValue]): XSLTContext = {
    XSLTContext(node, nodeList, position, globalVariables, newVariables)
  }
}
