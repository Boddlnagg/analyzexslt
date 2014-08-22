case class XSLTContext(node: XMLNode, nodeList: List[XMLNode], position: Int)

object TemplateEvaluator {
  def evaluate(tmpl: XSLTTemplate, context: XSLTContext) : List[XMLNode] = {
    evaluate(tmpl.content, context)
  }

  def evaluate(nodes: Seq[XSLTNode], context: XSLTContext) : List[XMLNode] = {
    nodes.flatMap(n => evaluate(n, context)).toList
  }

  def evaluate(node: XSLTNode, context: XSLTContext): List[XMLNode] = {
    node match {
      case LiteralElement(name, attributes, children) =>
        val resultNodes = children.flatMap(n => evaluate(n, context))
        // attributes must come before all other result nodes, afterwards they are ignored (see spec section 7.1.3)
        val resultAttributes = attributes ++ resultNodes
          .takeWhile(n => n.isInstanceOf[XMLAttribute])
          .map(n => n.asInstanceOf[XMLAttribute])
          .map(attr => (attr.name, attr.value))
        val resultChildren = resultNodes.filter(n => !n.isInstanceOf[XMLAttribute])
          List(XMLElement(name,
            resultAttributes.map { case (name, value) => XMLAttribute(name, value)}.toSeq,
            resultChildren))
      case LiteralTextNode(text) => List(XMLTextNode(text))
      case SetAttributeElement(attribute, value) =>
        // merge the content of all text-node children to create the attribute value
        val textResult = evaluate(value, context)
          .filter(n => n.isInstanceOf[XMLTextNode])
          .map(n => n.asInstanceOf[XMLTextNode].value)
          .mkString("")
        List(XMLAttribute(attribute, textResult))
      case _ => throw new UnsupportedOperationException(f"Evaluation of $node is not supported.")
    }
  }
}
