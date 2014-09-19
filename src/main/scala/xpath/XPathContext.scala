package xpath

import xml.XMLNode

/** The context used for evaluating XPath expressions (see XPath spec section 1)
  *
  * @param node the context node
  * @param position the context position
  * @param size the context size
  * @param variables a set of variable bindings
  */
case class XPathContext(node: XMLNode, position: Int, size: Int, variables: Map[String, XPathValue])
