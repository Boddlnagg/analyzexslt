case class Context(node: XMLNode, nodeList: List[XMLNode], position: Int)

object TemplateEvaluator {
  def evaluate(tmpl: XSLTTemplate, context: Context) : List[XMLNode] = {
    tmpl.content.flatMap(n => evaluate(n, context)).toList
  }

  def evaluate(node: XSLTNode, context: Context): List[XMLNode] = {
    node match {
      case LiteralElement(name, attributes, children) => List(XMLElement(name,
        attributes.map { case (name, value) => XMLAttribute(name, value)}.toSeq,
        children.flatMap(n => evaluate(n, context))))
      case _ => throw new UnsupportedOperationException(f"Evaluation of $node is not supported.")
    }
  }
}
