abstract class XSLTNode
case class LiteralElement(name: String, attributes: Map[String, String], children: Seq[XSLTNode]) extends XSLTNode
case class LiteralTextNode(text: String) extends XSLTNode
case class VariableDefinitionElement(name: String, select: XPathExpr) extends XSLTNode
case class ApplyTemplatesElement(select: Option[XPathExpr], params: Map[String, XPathExpr]) extends XSLTNode
case class CallTemplateElement(name: String, params: Map[String, XPathExpr]) extends XSLTNode
case class SetAttributeElement(attribute: String, value: Seq[XSLTNode]) extends XSLTNode
case class ValueOfElement(select: XPathExpr) extends XSLTNode
case class ChooseElement(branches: Seq[(XPathExpr, Seq[XSLTNode])], otherwise: Option[Seq[XSLTNode]]) extends XSLTNode